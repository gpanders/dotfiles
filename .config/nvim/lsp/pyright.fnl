{:filetypes ["python"]
 :cmd ["pyright-langserver" "--stdio"]
 :root_markers ["pyproject.toml" "setup.py" "setup.cfg" "requirements.txt" "Pipfile" "pyrightconfig.json"]
 :settings {:python {:analysis {:diagnosticMode "openFilesOnly"
                                :logLevel "Warning"}}}}
