let md_equalprg="pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block"
let md_equalprg.=g:markdown_reference_links ? "-shortcut_reference_links\ --reference-links\ --reference-location=section" : ""
let md_equalprg.=g:markdown_hard_wrap ? "\ --columns=79\ --wrap=auto" : "\ --wrap=none"
let md_equalprg.="\ --atx-headers"

let &equalprg=md_equalprg

setlocal foldenable
setlocal foldlevelstart=0
setlocal foldmethod=expr
setlocal foldexpr=FoldLevelMarkdown()
setlocal conceallevel=1
setlocal nospell
nnoremap gf :GotoFile<CR>

