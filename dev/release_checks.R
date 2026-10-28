# Pre-release checklist for OpenRange. Source the sections one at a time from
# the package root. This folder is in .Rbuildignore and never ships.
#
# The GitHub Actions workflow (.github/workflows/R-CMD-check.yaml) already runs
# R CMD check on Linux/macOS/Windows for every push, so the steps here are the
# ones CRAN asks for beyond that: the --as-cran incoming checks, the
# database-down checks that got 0.0.2 flagged, URL and spelling checks, tarball
# size, and the remote CRAN-like builders.

# 1. Bookkeeping ------------------------------------------------------------
# Bump Version in DESCRIPTION, add an entry to NEWS, update cran-comments.md
# (its Test environments should list the runs below once they pass).
# usethis::use_version("patch")        # or "minor" / "major"
# usethis::use_release_issue()         # tickable checklist as a GitHub issue

# 2. Regenerate docs --------------------------------------------------------
devtools::document()

# 3. Local checks -----------------------------------------------------------
devtools::test()

# devtools::check() documents first, which section 2 already did, so skip it.
chk <- devtools::check(document = FALSE, cran = TRUE, incoming = TRUE, remote = TRUE)
stopifnot(length(chk$errors) == 0, length(chk$warnings) == 0)

# If that call dies instead of returning -- "System command 'Rcmd.exe' failed",
# or the session itself vanishing with exit 15 and no R-level error -- look at
# what else is running before suspecting the package. See the second gotcha.
#
# "unable to verify current time" is the local network, not the package.
#
# Two local gotchas on Windows, neither of them the package:
#  * Run the check from a SHORT path (e.g. C:/orchk). Under a deep working
#    directory the install step blows past the Windows path limit and R dies
#    with exit 15 and no error message.
#  * Another memory-hungry R job running at the same time will get this one
#    killed. Windows runs out of commit and tears down whichever R processes
#    are resident, with no R-level error and nothing tryCatch can catch. A
#    small idle R process is as likely a victim as a large one, which is what
#    makes the failures look random and unrelated to what the check was doing
#    at the time -- so the step it dies on tells you nothing.
#
#    Measured 2026-10-27: a trivial 60-second R script survived 1 of 4 runs
#    while the Verts clean_group.R cluster was running, and 6 of 6 after it
#    stopped -- the second set with the Claude desktop app open and an R CMD
#    check running alongside, so neither of those is the cause. Windows had
#    logged Resource-Exhaustion-Detector event 2004 on four earlier days
#    naming Rscript.exe at 15 to 45 GB of virtual memory.
#
#    So before a release check, give the machine to itself, or at least look:
#      Get-Process Rscript, R, Rterm | Select Id, @{n='MB';e={[int]($_.WorkingSet64/1MB)}}
#      Get-WinEvent -FilterHashtable @{LogName='System';
#        ProviderName='Microsoft-Windows-Resource-Exhaustion-Detector'} -MaxEvents 5
#    With nothing else competing, the check completes normally and none of the
#    retry machinery below is needed.
#
#    When you cannot clear the machine, dev/check_cran.ps1 retries from outside
#    R, which survives the calling session being killed:
#
#      & .\dev\check_cran.ps1 -Attempts 6
#
#    The remote builders in step 5 and the GitHub Actions runs are unaffected
#    either way, and are the ones to trust.

# URLs in Rd files, README and DESCRIPTION.
urlchecker::url_check()

# Spelling. There is no inst/WORDLIST yet: fix real typos, then run
# update_wordlist() to accept the remaining terms (taxon names, PostgreSQL...).
spelling::spell_check_package()
# spelling::update_wordlist()          # accept the current list

# Tarball must stay comfortably under CRAN's 5 MB limit. The vcr cassette in
# tests/fixtures is the bulk of it.
#
# vignettes = FALSE just saves time: the rendered vignette in inst/doc adds
# only about 10 KB, and the package is around 250 KB against a 5 MB limit, so
# skipping it costs this measurement nothing. Section 3's check() builds the
# full tarball if you want the exact figure. (An earlier version of this file
# justified the flag as a way round the vignette's database query dying; that
# was the wrong diagnosis -- see the second gotcha in section 3.)
tb <- devtools::build(path = tempdir(), vignettes = FALSE, quiet = TRUE)
cat(basename(tb), round(file.size(tb) / 1024), "KB\n")
stopifnot(file.size(tb) < 4 * 1024^2)

# 4. With the database down -------------------------------------------------
# CRAN flagged 0.0.2 because re-building the vignette failed when the server
# refused connections ("remaining connection slots are reserved for
# non-replication superuser connections"). CRAN policy: functions that use the
# internet must fail gracefully (a message, no error or warning).
#
# These tests cover the failure paths without needing the resource to be down.
devtools::test(filter = "graceful_failure")

# Every exported function must return NULL with a message, not error, when the
# database is unreachable. ranges_sql() is the single point all of them go
# through, so mocking it covers the lot.
devtools::load_all()
testthat::with_mocked_bindings(
  {
    td <- file.path(tempdir(), "or_down")
    dir.create(td, showWarnings = FALSE)
    stopifnot(
      is.null(OpenRange_load_species(species = "Pinus contorta")),
      is.null(OpenRange_get_stats()),
      is.null(OpenRange_get_license()),
      is.null(OpenRange_list_scenarios()),
      is.null(OpenRange_species(species = "Pinus contorta", directory = td))
    )
  },
  ranges_sql = function(...) .resource_unavailable("OpenRange database (mocked outage)"),
  .package = "OpenRange")

# The vignette queries the database and is rebuilt during CRAN's checks. Render
# it with the database unreachable: it must build, show a note, and display the
# remaining code without evaluating it.
devtools::load_all()
local({
  out <- tempfile("vig"); dir.create(out)
  testthat::with_mocked_bindings(
    rmarkdown::render("vignettes/OpenRange.rmd", output_dir = out, quiet = TRUE),
    ranges_sql = function(...) .resource_unavailable("OpenRange database (mocked outage)"),
    .package = "OpenRange")
  html <- file.path(out, "OpenRange.html")
  stopifnot(file.exists(html),
            any(grepl("was not available", readLines(html, warn = FALSE))))
  cat("vignette builds with the database down\n")
})

# The API functions go through .api_query() instead; point it at a dead port.
devtools::load_all()
stopifnot(is.null(.api_query("http://127.0.0.1:1/")))

# 5. Remote builders (results arrive by email / GitHub) ---------------------
devtools::check_win_devel()            # CRAN's Windows R-devel builder
devtools::check_win_release()
devtools::check_win_oldrelease()
# R-hub v2 runs in this repo's own GitHub Actions (.github/workflows/rhub.yaml).
# The platforms below are the three OS runners, current and patched R, and
# \donttest examples. The call dispatches the workflow and prints the run URL;
# the package must be committed and pushed first, since the run checks what is
# on GitHub.
#
# "atlas" and "nosuggests" are deliberately NOT in this list. Both are minimal
# Fedora containers without the PostgreSQL client headers, so RPostgreSQL --
# which OpenRange has in Depends -- cannot be built from source there:
#
#   RS-PostgreSQL.h:23:14: fatal error: libpq-fe.h: No such file or directory
#   ERROR: compilation failed for package 'RPostgreSQL'
#
# The check never starts, so those jobs go red for a reason that has nothing to
# do with this package (run 40553018879, 2026-10-27, where the other six were
# Status: OK). RPostgreSQL declares no SystemRequirements, which is why r-hub's
# sysreqs step does not install libpq for it; that is upstream's to fix. CRAN's
# own Fedora flavours do have libpq, so this is not a submission risk. Add them
# back only if you want to watch for that changing.
rhub::rhub_doctor()
rhub::rhub_check(platforms = c("linux", "windows", "macos",
                               "ubuntu-release", "ubuntu-next", "donttest"))

# 6. Submit -----------------------------------------------------------------
# devtools::release()                  # interactive checklist, then uploads
# After acceptance: usethis::use_github_release(); usethis::use_dev_version()
