# Regenerates the screenshots used by duckdt-intro.qmd.
#
#   Rscript inst/slides/make-screenshots.R
#
# Needs the chromote package and a Chrome/Edge install. Only worth re-running
# when the explorer page itself changes.

library(duckdt)
source(file.path("inst", "slides", "demo-db.R"))

img_dir <- file.path("inst", "slides", "img")
dir.create(img_dir, showWarnings = FALSE, recursive = TRUE)

con <- demo_address_db()
on.exit(duckdt_disconnect(con))

page <- duckdt_erd(
  con,
  include_row_counts = TRUE,
  open = FALSE,
  file = file.path(tempdir(), "duckdt-erd-slides.html")
)

b <- chromote::ChromoteSession$new(width = 1500, height = 950)
on.exit(b$close(), add = TRUE)

# The slides are light, so ask the page for its light theme.
b$Emulation$setEmulatedMedia(
  features = list(list(name = "prefers-color-scheme", value = "light"))
)
b$Page$navigate(paste0("file:///", gsub("\\\\", "/", normalizePath(page))))
b$Page$loadEventFired()
Sys.sleep(4)

# Open one table's column list, so the screenshot shows the column picker.
b$Runtime$evaluate("
  var item = Array.prototype.find.call(
    document.querySelectorAll('.table-item'),
    function (i) { return i.dataset.table === 'addresses'; }
  );
  if (item) item.classList.add('open');
")
Sys.sleep(1)

b$screenshot(filename = file.path(img_dir, "explorer.png"))
message("wrote ", file.path(img_dir, "explorer.png"))
