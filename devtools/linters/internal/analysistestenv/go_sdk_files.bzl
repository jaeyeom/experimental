"""Exposes the execution platform's Go SDK files for analysistest targets."""

def _go_sdk_files_impl(ctx):
    sdk = ctx.toolchains["@rules_go//go:toolchain"].sdk
    sdk_files = depset(
        [sdk.go],
        transitive = [sdk.headers, sdk.srcs, sdk.libs, sdk.tools],
    )
    return [
        DefaultInfo(
            files = sdk_files,
            runfiles = ctx.runfiles(transitive_files = sdk_files),
        ),
    ]

go_sdk_files = rule(
    implementation = _go_sdk_files_impl,
    toolchains = ["@rules_go//go:toolchain"],
)
