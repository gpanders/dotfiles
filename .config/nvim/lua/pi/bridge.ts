import { createHash } from "node:crypto";
import { chmodSync, constants, rmSync } from "node:fs";
import { access, readFile } from "node:fs/promises";
import { createServer, type Server, type Socket } from "node:net";
import { homedir } from "node:os";
import { resolve } from "node:path";
import { isDeepStrictEqual } from "node:util";
import {
  createEditToolDefinition,
  type EditOperations,
  type ExtensionAPI,
  generateDiffString,
  generateUnifiedPatch,
  truncateHead,
} from "@earendil-works/pi-coding-agent";
import type { TSchema } from "@earendil-works/pi-ai";
import { Box, Text } from "@earendil-works/pi-tui";

const socketPath = process.env.PI_NVIM_SOCKET;
const maxMessageBytes = 1024 * 1024;
const maxEditorMessageBytes = 64 * 1024 * 1024;
const readTimeoutMs = 30_000;
const writeTimeoutMs = 120_000;

type Selection = {
  file: string;
  lines: { start: number; end: number };
  text: string;
};

type Location = Pick<Selection, "file" | "lines">;

type EditorTool = {
  name: string;
  label: string;
  description: string;
  parameters: TSchema;
  retrySafe: boolean;
};

type Request = {
  type?: unknown;
  request?: unknown;
  selection?: unknown;
  deliverAs?: unknown;
};

type EditorRevision = {
  buffer: number;
  changedtick: number;
  fileformat: string;
  fileencoding: string;
  binary: boolean;
  bomb: boolean;
  endofline: boolean;
  fixendofline: boolean;
};

type EditorResponse = {
  type?: unknown;
  requestId?: unknown;
  ok?: unknown;
  error?: unknown;
  content?: unknown;
  revision?: unknown;
  changedtick?: unknown;
  digest?: unknown;
  tools?: unknown;
  text?: unknown;
  details?: unknown;
};

type PendingRequest = {
  socket: Socket;
  operation: string;
  mutating: boolean;
  timer?: NodeJS.Timeout;
  resolve: (response: EditorResponse) => void;
  reject: (error: Error) => void;
};

function toolPath(value: unknown, cwd: string) {
  if (typeof value !== "string") return value;
  let path = value.replace(/[\u00a0\u2000-\u200a\u202f\u205f\u3000]/g, " ");
  if (path.startsWith("@")) path = path.slice(1);
  if (path === "~" || path.startsWith("~/")) path = `${homedir()}${path.slice(1)}`;
  return resolve(cwd, path);
}

function isSelection(value: unknown): value is Selection {
  if (!value || typeof value !== "object") return false;
  const selection = value as Partial<Selection>;
  const lines = selection.lines;
  return (
    typeof selection.file === "string" &&
    typeof selection.text === "string" &&
    !!lines &&
    typeof lines.start === "number" &&
    typeof lines.end === "number"
  );
}

function isRevision(value: unknown): value is EditorRevision {
  if (!value || typeof value !== "object") return false;
  const revision = value as Partial<EditorRevision>;
  return (
    typeof revision.buffer === "number" &&
    typeof revision.changedtick === "number" &&
    typeof revision.fileformat === "string" &&
    typeof revision.fileencoding === "string" &&
    typeof revision.binary === "boolean" &&
    typeof revision.bomb === "boolean" &&
    typeof revision.endofline === "boolean" &&
    typeof revision.fixendofline === "boolean"
  );
}

function parseEditorTools(value: unknown): EditorTool[] {
  if (!Array.isArray(value) || value.length > 32) throw new Error("The editor returned an invalid tool catalog");
  if (Buffer.byteLength(JSON.stringify(value)) > 512 * 1024) throw new Error("The editor tool catalog is too large");

  const names = new Set<string>();
  return value.map((entry) => {
    if (!entry || typeof entry !== "object" || Array.isArray(entry)) {
      throw new Error("The editor returned an invalid tool definition");
    }
    const tool = entry as Record<string, unknown>;
    if (
      typeof tool.name !== "string" ||
      !/^[a-z][a-z0-9_]{0,63}$/.test(tool.name) ||
      typeof tool.label !== "string" ||
      tool.label.length === 0 ||
      tool.label.length > 100 ||
      typeof tool.description !== "string" ||
      tool.description.length === 0 ||
      tool.description.length > 4000 ||
      typeof tool.retrySafe !== "boolean" ||
      !tool.parameters ||
      typeof tool.parameters !== "object" ||
      Array.isArray(tool.parameters) ||
      (tool.parameters as Record<string, unknown>).type !== "object"
    ) {
      throw new Error(`The editor returned an invalid tool definition: ${String(tool.name ?? "unknown")}`);
    }
    if (names.has(tool.name)) throw new Error(`The editor returned duplicate tool: ${tool.name}`);
    names.add(tool.name);
    return {
      name: tool.name,
      label: tool.label,
      description: tool.description,
      retrySafe: tool.retrySafe,
      parameters: tool.parameters as TSchema,
    };
  });
}

function truncateEditorText(text: string) {
  const truncated = truncateHead(text);
  if (!truncated.truncated) return text;
  const notice = `[Output truncated: ${truncated.outputLines} of ${truncated.totalLines} lines, ${truncated.outputBytes} of ${truncated.totalBytes} bytes.]`;
  return truncated.content ? `${truncated.content}\n\n${notice}` : notice;
}

function normalizeEditContent(content: string) {
  return content.replace(/^\uFEFF/, "").replace(/\r\n|\r/g, "\n");
}

export default function (pi: ExtensionAPI) {
  if (!socketPath) return;
  const bridgePath = socketPath;

  let server: Server | undefined;
  let editor: Socket | undefined;
  let nextRequestId = 1;
  const clients = new Set<Socket>();
  const pendingRequests = new Map<string, PendingRequest>();
  const completedEdits = new Map<string, number>();
  let editorToolCatalog: EditorTool[] | undefined;

  function pendingError(pending: PendingRequest, error: string) {
    return pending.mutating
      ? new Error(`${error}. The ${pending.operation} outcome is unknown. Do not retry this request`)
      : new Error(error);
  }

  function rejectPending(error: string, socket?: Socket) {
    for (const [id, pending] of pendingRequests) {
      if (!socket || pending.socket === socket) {
        pendingRequests.delete(id);
        clearTimeout(pending.timer);
        pending.reject(pendingError(pending, error));
      }
    }
  }

  function setEditor(socket: Socket) {
    if (editor && editor !== socket) {
      rejectPending("Neovim reconnected before the request completed", editor);
      editor.destroy();
    }
    editor = socket;
  }

  function broadcast(message: unknown) {
    editor?.write(`${JSON.stringify(message)}\n`);
  }

  function reply(socket: Socket, error?: string) {
    socket.end(`${JSON.stringify(error ? { error } : { ok: true })}\n`);
  }

  function requestEditor(message: Record<string, unknown>, options: { retrySafe?: boolean } = {}) {
    const socket = editor;
    if (!socket || socket.destroyed) {
      return Promise.reject(new Error("Neovim is not connected"));
    }

    const requestId = String(nextRequestId++);
    const operation = typeof message.operation === "string" ? message.operation : "unknown";
    const mutating = options.retrySafe === false || operation === "write";
    return new Promise<EditorResponse>((resolve, reject) => {
      const pending: PendingRequest = { socket, operation, mutating, resolve, reject };
      pending.timer = setTimeout(
        () => {
          if (mutating) {
            rejectPending(`Neovim did not answer the ${operation} request`, socket);
            if (editor === socket) editor = undefined;
            socket.destroy();
          } else if (pendingRequests.delete(requestId)) {
            reject(new Error(`Neovim did not answer the ${operation} request`));
          }
        },
        mutating ? writeTimeoutMs : readTimeoutMs,
      );
      pendingRequests.set(requestId, pending);
      socket.write(`${JSON.stringify({ ...message, type: "nvim_request", requestId })}\n`, (error) => {
        if (!error) return;
        if (!pendingRequests.delete(requestId)) return;
        clearTimeout(pending.timer);
        reject(pendingError(pending, error.message));
      });
    });
  }

  function acceptEditorResponse(socket: Socket, response: EditorResponse) {
    if (socket !== editor || typeof response.requestId !== "string") return;
    const pending = pendingRequests.get(response.requestId);
    if (!pending || pending.socket !== socket) return;
    pendingRequests.delete(response.requestId);
    clearTimeout(pending.timer);
    if (response.ok === true) {
      pending.resolve(response);
    } else {
      pending.reject(new Error(typeof response.error === "string" ? response.error : "Neovim request failed"));
    }
  }

  async function stop() {
    rejectPending("Neovim bridge stopped");
    editor = undefined;
    completedEdits.clear();
    for (const client of clients) client.destroy();
    clients.clear();
    const current = server;
    server = undefined;
    if (current?.listening) {
      await new Promise<void>((resolve) => current.close(() => resolve()));
    }
    rmSync(bridgePath, { force: true });
  }

  function registerEditTool(cwd: string) {
    const base = createEditToolDefinition(cwd);
    pi.registerTool({
      ...base,
      async execute(toolCallId, params, signal, onUpdate, ctx) {
        let original: Buffer | undefined;
        let actual: Buffer | undefined;
        let revision: EditorRevision | undefined;
        const operations: EditOperations = {
          access: (path) => access(path, constants.R_OK | constants.W_OK),
          readFile: async (path) => {
            const response = await requestEditor({ operation: "read", toolCallId, path });
            if (typeof response.content !== "string" || !isRevision(response.revision)) {
              throw new Error("Neovim returned an invalid edit snapshot");
            }
            const content = Buffer.from(response.content, "utf8");
            const disk = await readFile(path);
            if (!content.equals(disk)) {
              throw new Error(`Neovim buffer does not match ${path} on disk`);
            }
            original = content;
            revision = response.revision;
            return content;
          },
          writeFile: async (path, content) => {
            if (!original || !revision) throw new Error("Neovim edit has no source snapshot");
            if (!(await readFile(path)).equals(original)) {
              throw new Error(`${path} changed after Neovim prepared the edit`);
            }
            const response = await requestEditor({
              operation: "write",
              toolCallId,
              path,
              content,
              revision,
            });
            if (typeof response.digest !== "string" || typeof response.changedtick !== "number") {
              throw new Error("Neovim returned an invalid edit result. Do not retry this edit");
            }
            actual = await readFile(path);
            if (createHash("sha256").update(actual).digest("hex") !== response.digest) {
              throw new Error(`Neovim saved ${path}, but the file changed again. Do not retry this edit`);
            }
            completedEdits.set(toolCallId, response.changedtick);
          },
        };
        const edit = createEditToolDefinition(cwd, { operations });
        const result = await edit.execute(toolCallId, params, signal, onUpdate, ctx);
        if (original && actual) {
          const before = normalizeEditContent(original.toString("utf8"));
          const after = normalizeEditContent(actual.toString("utf8"));
          const generated = generateDiffString(before, after);
          result.details = {
            diff: generated.diff,
            patch: generateUnifiedPatch(params.path, before, after),
            firstChangedLine: generated.firstChangedLine,
          };
          if (before === after) {
            result.content = [{ type: "text", text: `Neovim write hooks left ${params.path} unchanged.` }];
          }
        }
        return result;
      },
    });
  }

  function registerEditorTools(tools: EditorTool[]) {
    const existing = new Set(pi.getAllTools().map((tool) => tool.name));
    for (const tool of tools) {
      if (existing.has(tool.name)) throw new Error(`Editor tool conflicts with an existing tool: ${tool.name}`);
    }
    for (const tool of tools) {
      pi.registerTool({
        name: tool.name,
        label: tool.label,
        description: tool.description,
        parameters: tool.parameters,
        async execute(toolCallId, params, _signal, _onUpdate, ctx) {
          const response = await requestEditor(
            {
              operation: "tools/execute",
              tool: tool.name,
              arguments: params,
              cwd: ctx.cwd,
              toolCallId,
            },
            { retrySafe: tool.retrySafe },
          );
          if (typeof response.text !== "string") {
            throw new Error(`The editor returned an invalid ${tool.name} result`);
          }
          return {
            content: [{ type: "text", text: truncateEditorText(response.text) }],
            details: response.details,
          };
        },
      });
    }
  }

  async function subscribeEditor(socket: Socket) {
    setEditor(socket);
    try {
      const response = await requestEditor({ operation: "tools/list" });
      const tools = parseEditorTools(response.tools);
      if (editorToolCatalog) {
        if (!isDeepStrictEqual(tools, editorToolCatalog)) {
          throw new Error("The editor tool catalog changed. Reload Pi to use the new catalog");
        }
      } else {
        registerEditorTools(tools);
        editorToolCatalog = tools;
      }
      if (editor === socket) socket.write(`${JSON.stringify({ type: "subscribed" })}\n`);
    } catch (error) {
      if (editor === socket) editor = undefined;
      if (!socket.destroyed) {
        socket.write(
          `${JSON.stringify({
            type: "subscription_error",
            error: error instanceof Error ? error.message : String(error),
          })}\n`,
        );
      }
    }
  }

  pi.registerMessageRenderer("neovim-prompt", (message, { outputPad }, theme) => {
    const details = message.details as { request?: string; selection?: Location } | undefined;
    const selection = details?.selection;
    const request = details?.request ?? "";
    const text = selection
      ? `${theme.fg("muted", `${selection.file}:${selection.lines.start}-${selection.lines.end}`)}\n\n${request}`
      : request;
    const box = new Box(outputPad, 1, (text) => theme.bg("userMessageBg", text));
    box.addChild(new Text(text, 0, 0));
    return box;
  });

  pi.on("message_start", (event) => {
    const external = event.message.role === "custom" && event.message.customType === "neovim-prompt";
    if (event.message.role === "assistant") {
      broadcast({ type: "assistant_start" });
    } else if (event.message.role === "user" || external) {
      broadcast({ type: "prompt_start", external });
    }
  });

  pi.on("agent_settled", () => broadcast({ type: "agent_settled" }));

  pi.on("tool_execution_start", (event, ctx) => {
    const args = event.args ?? {};
    broadcast({
      type: "tool_start",
      id: event.toolCallId,
      tool: event.toolName,
      args: {
        path: toolPath(args.path, ctx.cwd),
        offset: args.offset,
        limit: args.limit,
      },
    });
  });

  pi.on("tool_result", (event, ctx) => {
    if (event.toolName !== "read" && event.toolName !== "edit" && event.toolName !== "write") return;
    const changedtick = completedEdits.get(event.toolCallId);
    completedEdits.delete(event.toolCallId);
    broadcast({
      type: "tool_end",
      id: event.toolCallId,
      tool: event.toolName,
      path: toolPath(event.input.path, ctx.cwd),
      isError: event.isError,
      details: event.toolName === "read" ? event.details : undefined,
      changedtick,
      isImage: event.content.some((item) => item.type === "image"),
    });
  });

  pi.on("session_start", async (_event, ctx) => {
    if (ctx.mode !== "tui") return;

    try {
      await stop();
      server = createServer((socket) => {
        clients.add(socket);
        let input = "";

        const disconnect = () => {
          clients.delete(socket);
          if (editor === socket) {
            editor = undefined;
            rejectPending("Neovim disconnected before the request completed", socket);
          }
        };

        socket.setEncoding("utf8");
        socket.on("close", disconnect);
        socket.on("error", disconnect);
        socket.on("data", (chunk) => {
          input += chunk;

          while (true) {
            const limit = socket === editor ? maxEditorMessageBytes : maxMessageBytes;
            const newline = input.indexOf("\n");
            if (newline < 0) {
              if (Buffer.byteLength(input) > limit) reply(socket, "message is too large");
              return;
            }
            const line = input.slice(0, newline);
            input = input.slice(newline + 1);
            if (Buffer.byteLength(line) > limit) {
              reply(socket, "message is too large");
              return;
            }

            let value: unknown;
            try {
              value = JSON.parse(line);
            } catch {
              reply(socket, "invalid JSON");
              return;
            }
            if (!value || typeof value !== "object" || Array.isArray(value)) {
              reply(socket, "invalid message");
              return;
            }

            const message = value as Request & EditorResponse;
            if (message.type === "subscribe") {
              void subscribeEditor(socket);
              continue;
            }
            if (message.type === "nvim_response") {
              acceptEditorResponse(socket, message);
              continue;
            }
            if (message.type !== "prompt" || typeof message.request !== "string") {
              reply(socket, "invalid prompt");
              return;
            }
            if (message.selection !== undefined && !isSelection(message.selection)) {
              reply(socket, "invalid selection");
              return;
            }

            const deliverAs = message.deliverAs === "followUp" ? "followUp" : "steer";
            const content = message.selection
              ? JSON.stringify({
                  schema: "pi-neovim-selection",
                  selection: message.selection,
                  request: message.request,
                })
              : message.request;
            try {
              pi.sendMessage(
                {
                  customType: "neovim-prompt",
                  content,
                  display: true,
                  details: {
                    request: message.request,
                    selection: message.selection && {
                      file: message.selection.file,
                      lines: message.selection.lines,
                    },
                  },
                },
                { triggerTurn: true, deliverAs },
              );
              reply(socket);
            } catch (error) {
              reply(socket, error instanceof Error ? error.message : String(error));
            }
            return;
          }
        });
      });
      const activeServer = server;
      activeServer.on("error", (error) => ctx.ui.notify(`Neovim bridge: ${error.message}`, "error"));
      await new Promise<void>((resolve, reject) => {
        activeServer.once("error", reject);
        activeServer.listen(bridgePath, () => {
          activeServer.off("error", reject);
          try {
            chmodSync(bridgePath, 0o600);
            resolve();
          } catch (error) {
            reject(error);
          }
        });
      });
      registerEditTool(ctx.cwd);
    } catch (error) {
      await stop();
      ctx.ui.notify(`Neovim bridge: ${error instanceof Error ? error.message : String(error)}`, "error");
    }
  });

  pi.on("session_shutdown", async () => stop());
}
