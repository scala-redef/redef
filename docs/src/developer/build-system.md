# Build System for Scala Redef

## Build and test

Bazel is the source of truth for the JVM library, tests, dependency lock, and documentation:

```sh
bazel build //...
bazel test //...
bazel build //docs:site
```

The generated Scaladoc site is at `bazel-bin/docs/site/index.html`, with API documentation under `api/`.

Maven dependencies are pinned in `maven_install.json`. After changing the artifact list in `MODULE.bazel`, refresh it with:

```sh
REPIN=1 bazel run @maven//:pin
```
