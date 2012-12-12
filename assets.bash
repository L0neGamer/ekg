#!/bin/bash
set -o errexit -o nounset -o pipefail

function code {
  find assets -type f | while read path
  do
    route="${path#assets/}"
    case "$path" in *.js)   type=application/javascript ;;
                    *.css)  type=text/css ;;
                    *.png)  type=image/png ;;
                    *.html) type=text/html ;;
                    esac
    space="$(tr -c '\n' ' ' <<<"$route")"
cat <<ROUTE
        , ("$route", method GET (modifyResponse (setContentType "$type") >>
            $space               writeBS \$(embedFile "$path")))
ROUTE
  done
}

code "$@"
