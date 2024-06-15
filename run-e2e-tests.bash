#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

main () {
    local this_script_path=$(realpath $0)
    local project_dir=$(dirname "$this_script_path")
    if [[ ! -d $project_dir/venv ]]; then
        python3 -m venv "$project_dir/venv"
        "$project_dir/venv/bin/pip" install -r $project_dir/e2e/requirements.txt
    fi
    $project_dir/venv/bin/pytest "$@"
}

main "$@"
