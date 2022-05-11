# flake-to-gitlab-ci

Converts a flake into gitlab CI jobs using GitLab dynamic pipeline.

To use this you need to make a `.gitlab-ci.yml` file in your gitlab repository
with the following contents:

```
stages:
- generate
- check

generate-flake-ci:
  stage: generate
  script: nix run "git+https://gitlab.homotopic.tech/haskell/flake-to-gitlab-ci" > flake-ci.yml
  artifacts:
    paths:
      - flake-ci.yml

flake-ci:
  stage: check
  trigger:
    include:
      - artifact: flake-ci.yml
        job: generate-flake-ci
    strategy: depend
```
