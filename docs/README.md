# Work with the documentation website

This guide assumes Bazelisk is installed. The repository pins Bazel in `.bazelversion`.

## Build the site

```sh
bazel build //docs:site
```

Scala 3 Scaladoc writes the static website to `bazel-bin/docs/site/`.

## Preview locally

```sh
python3 -m http.server --directory bazel-bin/docs/site 8000
```

Open <http://localhost:8000/>.

## Check the site

```sh
bazel test //docs:site_test
```

The check requires the site entry point, a prose page, and API pages from both modules.

## Add a page

1. Add a Markdown file under `docs/src/`.
2. Add the `src/<file>.md` label to `pages` on `//docs:site` in `docs/BUILD.bazel`.
3. Add the page to `docs/src/sidebar.yml`.
4. Run `bazel build //docs:site`.
