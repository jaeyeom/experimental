---
trigger: model_decision
description: Use this rule if you see bazel related files such as `MODULE.bazel` file in the repo root.
---

1. Run `bazel run //:gazelle` to generate `BUILD.bazel` files automatically.
2. Run `bazel run //:format && bazel test //path/to/your/project/...:all` whenever you make a change.
