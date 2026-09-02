"""Minimal Scala 3 Scaladoc static-site rules."""

load("@rules_java//java/common:java_info.bzl", "JavaInfo")

def _stage_file(ctx, source, path):
    output = ctx.actions.declare_file("{}_siteroot/{}".format(ctx.label.name, path))
    ctx.actions.expand_template(output = output, template = source, substitutions = {})
    return output

def _scaladoc_site_impl(ctx):
    jars = [dep[JavaInfo].outputs.jars[0].class_jar for dep in ctx.attr.deps]
    classpath = depset(transitive = [dep[JavaInfo].transitive_compile_time_jars for dep in ctx.attr.deps])
    extracted = []

    for index, jar in enumerate(jars):
        output = ctx.actions.declare_directory("{}_tasty_{}".format(ctx.label.name, index))
        ctx.actions.run(
            arguments = ["x", jar.path, "-d", output.path],
            executable = ctx.file._zipper,
            inputs = [jar],
            mnemonic = "ExtractTasty",
            outputs = [output],
            tools = [ctx.file._zipper],
        )
        extracted.append(output)
    index = _stage_file(ctx, ctx.file.index, "_docs/index.md")
    sidebar = _stage_file(ctx, ctx.file.sidebar, "sidebar.yml")
    staged = [
        index,
        sidebar,
    ]
    seen = {"index.md": True}

    for page in ctx.files.pages:
        if page.basename in seen:
            fail("duplicate documentation page: {}".format(page.basename))
        seen[page.basename] = True
        staged.append(_stage_file(ctx, page, "_docs/{}".format(page.basename)))

    output = ctx.actions.declare_directory(ctx.label.name)
    args = ctx.actions.args()
    args.add("-project")
    args.add(ctx.attr.project_name)
    args.add("-experimental")
    args.add("-usejavacp")
    args.add("-d")
    args.add_all([output], expand_directories = False)
    args.add("-siteroot")
    args.add(sidebar.dirname)
    args.add("-Yapi-subdirectory")
    args.add("-comment-syntax:markdown")
    args.add("-social-links:github::{}".format(ctx.attr.repository_url))
    args.add_joined("-classpath", classpath, join_with = ctx.configuration.host_path_separator)
    args.add_all(extracted)

    ctx.actions.run(
        arguments = [args],
        executable = ctx.executable.scaladoc,
        inputs = depset(direct = staged + extracted, transitive = [classpath]),
        mnemonic = "ScalaDocSite",
        outputs = [output],
        progress_message = "Generating Scaladoc site %{label}",
        tools = [ctx.attr.scaladoc[DefaultInfo].files_to_run],
    )

    return [DefaultInfo(files = depset([output]))]

scaladoc_site = rule(
    implementation = _scaladoc_site_impl,
    attrs = {
        "deps": attr.label_list(mandatory = True, providers = [JavaInfo]),
        "index": attr.label(allow_single_file = [".md"], mandatory = True),
        "pages": attr.label_list(allow_files = [".md"]),
        "project_name": attr.string(mandatory = True),
        "repository_url": attr.string(
            default = "https://github.com/scala-redef/scala-redef",
        ),
        "scaladoc": attr.label(
            cfg = "exec",
            executable = True,
            mandatory = True,
            providers = [JavaInfo],
        ),
        "sidebar": attr.label(allow_single_file = [".yml", ".yaml"], mandatory = True),
        "_zipper": attr.label(
            allow_single_file = True,
            cfg = "exec",
            default = "@bazel_tools//tools/zip:zipper",
        ),
    },
)

def _scaladoc_site_test_impl(ctx):
    site = ctx.file.site
    executable = ctx.actions.declare_file(ctx.label.name + ".sh")
    ctx.actions.write(
        content = "#!/usr/bin/env bash\nset -euo pipefail\nsite=\"${TEST_SRCDIR}/${TEST_WORKSPACE}/%s\"\ntest -f \"${site}/index.html\"\ntest -f \"${site}/why.html\"\ntest -f \"${site}/api/index.html\"\ntest -f \"${site}/api/redef/data/Ok.html\"\ntest -f \"${site}/api/redef/tls/params/CipherSuite.html\"\n" % site.short_path,
        is_executable = True,
        output = executable,
    )
    return [DefaultInfo(
        executable = executable,
        runfiles = ctx.runfiles(files = [site]),
    )]

scaladoc_site_test = rule(
    implementation = _scaladoc_site_test_impl,
    attrs = {
        "site": attr.label(allow_single_file = True, mandatory = True),
    },
    test = True,
)
