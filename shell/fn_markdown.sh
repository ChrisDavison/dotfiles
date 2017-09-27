#! /usr/bin/env bash
mdpreview() {
    pandoc -s -c ~/.dotfiles/simple-pandoc-css.css --mathjax $1 > ~/.mdpreview.html && open ~/.mdpreview.html
}

mdpreviewself() {
    pandoc -s -c ~/.dotfiles/simple-pandoc-css.css --self-contained --mathjax $1 > ~/.mdpreview.html && open ~/.mdpreview.html
}

mdtohtml() {
    fn=.$(extractFilenameNoExt $1).html
    [ -e $fn ] && rm $fn
    pandoc -s -c ~/.dotfiles/github-markdown.css --self-contained --mathjax $1 > $fn
}

md2pdf() {
    outfn="$1"
    title="$2"
    pandoc ${@:3:$#} -o $outfn -S --toc --toc-depth=2 -V title=$title -V documentclass=report -V geometry:margin=1in
}

mdtidy() {
    fmt="markdown_github-hard_line_breaks+yaml_metadata_block+tex_math_dollars+line_blocks"
    pandoc -s --atx-headers --normalize -t $fmt --wrap=none $1 -o $1
}

mdtidywrap() {
    fmt="markdown_github-hard_line_breaks+yaml_metadata_block+tex_math_dollars+line_blocks"
    pandoc -s --atx-headers --normalize -t $fmt --columns=80 $1 -o $1
}

mdtidyref() {
    fmt="markdown_github-hard_line_breaks+yaml_metadata_block+tex_math_dollars+line_blocks"
    pandoc -s --atx-headers --normalize -t $fmt --columns=80 --reference-links $1 -o $1
}