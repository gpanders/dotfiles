(autocmd :my-snippets :InsertEnter "*" :once
  (local (ok snippets) (pcall require :snippets))
  (when ok
    (keymap :i "<Tab>" "v:lua.check_snippet() ? '<Cmd>lua require(\"snippets\").expand_or_advance(1)<CR>' : '<Tab>'" {:expr true})
    (keymap :i "<S-Tab>" "<Cmd>lua require('snippets').advance_snippet(-1)<CR>")

    (global check_snippet (fn []
      (if (snippets.has_active_snippet)
          true
          (let [(_ snippet) (snippets.lookup_snippet_at_cursor)]
            (not= snippet nil)))))

    (global split_getopts (fn [str]
      (-> (icollect [c (str:gmatch "(%a[:]?)")]
                    (if (c:find ":")
                        (string.format "\t\t%s) echo \"$OPTARG\" ;;" (c:sub 1 1))
                        (string.format "\t\t%s) ;;" c)))
          (table.concat "\n"))))

    (local U (require "snippets.utils"))

    (set snippets.ux (require "snippets.inserters.extmarks"))

    (local ext-ft-map {:py :python :rs :rust})
    (local include-map {:c [:cpp]})
    (local snippets-dir (.. (vim.fn.stdpath "config") "/snippets"))

    (fn read-snippets [t]
      (local s (or t {}))
      (each [filename (vim.gsplit (vim.fn.glob (.. snippets-dir "/*")) "\n")]
        (with-open [f (io.open filename)]
          (let [(name ext) (filename:match "^.+/([^/]+)%.([^.]+)$")
                ft (or (. ext-ft-map ext) ext)]
            (var snippet (string.gsub (f:read "*a") "\n$" ""))
            (when (snippet:find "\n")
              (set snippet (U.match_indentation snippet)))

            (when (not (. s ft))
              (tset s ft {}))
            (tset (. s ft) name snippet)

            (each [_ v (ipairs (or (. include-map ft) []))]
              (when (not (. s v))
                (tset s v {}))
              (tset (. s v) name snippet)))))
      s)

      (set snippets.snippets (read-snippets {
        :_global {
          :copyright (U.force_comment "Copyright (C) ${=os.date(\"%Y\")} Gregory Anders")
          :GPL
            (do
              (local S (U.force_comment "
Copyright (C) ${=os.date(\"%Y\")} Gregory Anders

SPDX-License-Identifier: GPL-3.0-or-later

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>."))
              (table.insert S "\n")
              (table.insert S "\n")
              S)
        }
      }))))
