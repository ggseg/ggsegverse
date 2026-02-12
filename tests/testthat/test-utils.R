describe("ggsegverse_packages", {
  it("returns core package names", {
    pkgs <- ggsegverse_packages(include_self = FALSE)
    expect_true("ggseg" %in% pkgs)
    expect_true("ggseg3d" %in% pkgs)
    expect_true("ggseg.formats" %in% pkgs)
  })

  it("includes self when requested", {
    pkgs <- ggsegverse_packages(include_self = TRUE)
    expect_true("ggsegverse" %in% pkgs)
  })
})

describe("ggsegverse_conflicts", {
  it("returns ggsegverse_conflicts class", {
    conflicts <- ggsegverse_conflicts()
    expect_s3_class(conflicts, "ggsegverse_conflicts")
  })
})
