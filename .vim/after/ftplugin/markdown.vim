let md_equalprg="pandoc\ --to\ markdown+pipe_tables-simple_tables-fenced_code_attributes+task_lists+yaml_metadata_block"
let md_equalprg.=g:markdown_reference_links ? "-shortcut_reference_links\ --reference-links\ --reference-location=section" : ""
let md_equalprg.=g:markdown_hard_wrap ? "\ --columns=79\ --wrap=auto" : "\ --wrap=none"
let md_equalprg.="\ --atx-headers"

let &equalprg=md_equalprg

set foldenable
set foldlevelstart=0
set foldmethod=expr
set foldexpr=FoldLevelMarkdown()
set conceallevel=1
set nospell
nnoremap gf :GotoFile<CR>

