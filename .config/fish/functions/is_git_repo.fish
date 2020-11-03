#!/usr/bin/env fish

function is_git_repo
    git rev-parse HEAD >/dev/null 2>&1
end
