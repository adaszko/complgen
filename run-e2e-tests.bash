#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

main () {
    local this_script_path=$(realpath $0)
    local project_dir=$(dirname "$this_script_path")
    pushd "$project_dir/e2e" >/dev/null
    uv --project "$project_dir/e2e" run py.test --no-header "$@"
    popd >/dev/null
}

main "$@"
