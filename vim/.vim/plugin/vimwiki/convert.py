#!/usr/bin/env python3

from os import path
import re
import shutil
import subprocess
import sys
import tempfile


def convert(
    force,
    syntax,
    extension,
    output_dir,
    input_file,
    css_file,
    template_path,
    template_default,
    template_ext,
    root_path,
    custom_args,
):
    if shutil.which("pandoc") is None:
        print("Error: pandoc not found", file=sys.stderr)
        sys.exit(1)

    if syntax != "markdown":
        print("Error: Unsupported syntax", file=sys.stderr)
        sys.exit(1)

    input_file_name = path.splitext(path.basename(input_file))[0]
    output_file = path.join(output_dir, input_file_name) + path.extsep + "html"

    with open(input_file, "r", encoding="utf8") as f:
        lines = re.sub(r"\[([^]]+)\]\((.+)\)", repl, f.read())

    # Extract page title from text, if possible. Otherwise just use the name of
    # the input file
    title = extract_title(lines) or input_file_name.title()

    # Build path to template file
    template = path.join(template_path, template_default + path.extsep + template_ext)

    # Build the command string
    command = [
        "pandoc",
        "--section-divs",
        "--template={}".format(template) if path.isfile(template) else "",
        "--standalone",
        "--katex",
        "--metadata",
        "pagetitle={}".format(title),
        custom_args if custom_args != "-" else "",
        "--from=markdown",
        "--to=html",
        "--output",
        output_file,
        "-",
    ]

    # Prune empty elements from command list
    command = list(filter(None, command))

    # Run command
    subprocess.run(command, check=True, encoding="utf8", input=lines)


def repl(match):
    link = path.splitext(match.group(2))[0] + ".html"
    return "[{}]({})".format(match.group(1), link)


def extract_title(text):
    """Try to extract the page title from metadata in the file contents. The
    file metadata can be in YAML form such as

    ---
    title: My title
    ---

    anywhere in the file or

    % title

    at the beginning. The title can also span multiple lines as long as each
    newline is preceded with a space.  All of these caveats are why the
    following regex is so complicated.
    """
    match = re.match(
        r"(?:^% (.+?)\n^[^ ]|(?:.*^\s*\n|)^-{3}\ntitle: ([^\n]+)\n.*^(?:-{3}|\.{3})$)",
        text,
        re.MULTILINE | re.DOTALL,
    )

    if match:
        return match.group(1) or match.group(2)


if __name__ == "__main__":
    convert(*sys.argv[1:])
