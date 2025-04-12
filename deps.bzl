load("@bazel_gazelle//:deps.bzl", "go_repository")

def go_dependencies():
    """Go dependencies for the experimental repository.

    This function will be populated by gazelle.
    """
    pass
    go_repository(
        name = "com_github_dustin_go_humanize",
        importpath = "github.com/dustin/go-humanize",
        sum = "h1:GzkhY7T5VNhEkwH0PVJgjz+fX1rhBrR7pRT3mDkpeCY=",
        version = "v1.0.1",
    )
    go_repository(
        name = "com_github_google_pprof",
        importpath = "github.com/google/pprof",
        sum = "h1:gbpYu9NMq8jhDVbvlGkMFWCjLFlqqEZjEmObmhUy6Vo=",
        version = "v0.0.0-20240409012703-83162a5b38cd",
    )
    go_repository(
        name = "com_github_google_uuid",
        importpath = "github.com/google/uuid",
        sum = "h1:NIvaJDMOsjHA8n1jAhLSgzrAzy1Hgr+hNrb57e+94F0=",
        version = "v1.6.0",
    )
    go_repository(
        name = "com_github_jaeyeom_sugo",
        importpath = "github.com/jaeyeom/sugo",
        sum = "h1:81K9ElfZJ3/dkcTBrugcSxHS25G1N6XeRYG7rjFn4yo=",
        version = "v1.1.1",
    )
    go_repository(
        name = "com_github_leanovate_gopter",
        importpath = "github.com/leanovate/gopter",
        sum = "h1:eFPtJ3aa5zLfbxGROSNY75T9Dume60CWBAqoWQ3h/ig=",
        version = "v0.2.8",
    )
    go_repository(
        name = "com_github_mattn_go_isatty",
        importpath = "github.com/mattn/go-isatty",
        sum = "h1:xfD0iDuEKnDkl03q4limB+vH+GxLEtL/jb4xVJSWWEY=",
        version = "v0.0.20",
    )
    go_repository(
        name = "com_github_ncruces_go_strftime",
        importpath = "github.com/ncruces/go-strftime",
        sum = "h1:bY0MQC28UADQmHmaF5dgpLmImcShSi2kHU9XLdhx/f4=",
        version = "v0.1.9",
    )
    go_repository(
        name = "com_github_remyoudompheng_bigfft",
        importpath = "github.com/remyoudompheng/bigfft",
        sum = "h1:W09IVJc94icq4NjY3clb7Lk8O1qJ8BdBEF8z0ibU0rE=",
        version = "v0.0.0-20230129092748-24d4a6f8daec",
    )
    go_repository(
        name = "org_golang_x_mod",
        importpath = "golang.org/x/mod",
        sum = "h1:QX4fJ0Rr5cPQCF7O9lh9Se4pmwfwskqZfq5moyldzic=",
        version = "v0.16.0",
    )
    go_repository(
        name = "org_golang_x_sys",
        importpath = "golang.org/x/sys",
        sum = "h1:RI27ohtqKCnwULzJLqkv897zojh5/DwS/ENaMzUOaWI=",
        version = "v0.22.0",
    )
    go_repository(
        name = "org_golang_x_tools",
        importpath = "golang.org/x/tools",
        sum = "h1:tfGCXNR1OsFG+sVdLAitlpjAvD/I6dHDKnYrpEZUHkw=",
        version = "v0.19.0",
    )
    go_repository(
        name = "org_modernc_cc_v4",
        importpath = "modernc.org/cc/v4",
        sum = "h1:3Be/Rdo1fpr8GrQ7IVw9OHtplU4gWbb+wNgeoBMmGLQ=",
        version = "v4.21.4",
    )
    go_repository(
        name = "org_modernc_ccgo_v4",
        importpath = "modernc.org/ccgo/v4",
        sum = "h1:lwQZgvboKD0jBwdaeVCTouxhxAyN6iawF3STraAal8Y=",
        version = "v4.19.2",
    )
    go_repository(
        name = "org_modernc_fileutil",
        importpath = "modernc.org/fileutil",
        sum = "h1:gQ5SIzK3H9kdfai/5x41oQiKValumqNTDXMvKo62HvE=",
        version = "v1.3.0",
    )
    go_repository(
        name = "org_modernc_gc_v2",
        importpath = "modernc.org/gc/v2",
        sum = "h1:9cNzOqPyMJBvrUipmynX0ZohMhcxPtMccYgGOJdOiBw=",
        version = "v2.4.1",
    )
    go_repository(
        name = "org_modernc_libc",
        importpath = "modernc.org/libc",
        sum = "h1:AzcW1mhlPNrRtjS5sS+eW2ISCgSOLLNyFzRh/V3Qj/U=",
        version = "v1.55.3",
    )
    go_repository(
        name = "org_modernc_mathutil",
        importpath = "modernc.org/mathutil",
        sum = "h1:fRe9+AmYlaej+64JsEEhoWuAYBkOtQiMEU7n/XgfYi4=",
        version = "v1.6.0",
    )
    go_repository(
        name = "org_modernc_memory",
        importpath = "modernc.org/memory",
        sum = "h1:IqGTL6eFMaDZZhEWwcREgeMXYwmW83LYW8cROZYkg+E=",
        version = "v1.8.0",
    )
    go_repository(
        name = "org_modernc_opt",
        importpath = "modernc.org/opt",
        sum = "h1:3XOZf2yznlhC+ibLltsDGzABUGVx8J6pnFMS3E4dcq4=",
        version = "v0.1.3",
    )
    go_repository(
        name = "org_modernc_sortutil",
        importpath = "modernc.org/sortutil",
        sum = "h1:jQiD3PfS2REGJNzNCMMaLSp/wdMNieTbKX920Cqdgqc=",
        version = "v1.2.0",
    )
    go_repository(
        name = "org_modernc_sqlite",
        importpath = "modernc.org/sqlite",
        sum = "h1:Bb6SR13/fjp15jt70CL4f18JIN7p7dnMExd+UFnF15g=",
        version = "v1.34.5",
    )
    go_repository(
        name = "org_modernc_strutil",
        importpath = "modernc.org/strutil",
        sum = "h1:agBi9dp1I+eOnxXeiZawM8F4LawKv4NzGWSaLfyeNZA=",
        version = "v1.2.0",
    )
    go_repository(
        name = "org_modernc_token",
        importpath = "modernc.org/token",
        sum = "h1:Xl7Ap9dKaEs5kLoOQeQmPWevfnk/DM5qcLcYlA8ys6Y=",
        version = "v1.1.0",
    )
