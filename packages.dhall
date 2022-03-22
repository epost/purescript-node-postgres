let upstream =
      https://raw.githubusercontent.com/working-group-purescript-es/package-sets/main/packages.dhall
        sha256:bd4871410dc601d8d634ff1b09bbcdb048c8eb192b1cbbfc0f3271e92329d363

in  upstream
  with metadata.version = "v0.15.0-alpha-02"
