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

    output_file = "{}/{}.html".format(
        output_dir, path.splitext(path.basename(input_file))[0]
    )

    with open(input_file, "r", encoding="utf8") as f:
        lines = f.read()

    lines = re.sub(r"\[([^]]+)\]\((.+)\)", repl, lines)
    subprocess.run(
        [
            "pandoc",
            "--section-divs",
            "--template",
            path.join(template_path, template_default + path.extsep + template_ext),
            "-f",
            "markdown",
            "-t",
            "html",
            "-o",
            output_file,
            "-",
        ],
        check=True,
        encoding="utf8",
        input=lines,
    )


def repl(match):
    link = path.splitext(match.group(2))[0] + ".html"
    return "[{}]({})".format(match.group(1), link)


if __name__ == "__main__":
    convert(*sys.argv[1:])
