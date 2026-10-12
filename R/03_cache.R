# ---- Shared result cache ----
#
# The data helpers below cache what they read (a subbasin's series, a
# chemical's source apportionment, a download summary, ...) so a selection
# revisited by anyone is served from memory. The app runs as one long-lived
# process (a single container) serving every session, so these caches are
# shared by all users and live as long as the container does. Kept in
# separate unbounded environments they only ever grew, with every selection
# anyone visited. They now share this one cache instead: at most
# ECOMIX_CACHE_MB megabytes (512 by default, as measured by object.size()),
# evicting the least recently used entries first. cachem empties the whole
# cache when a single value is bigger than it, so keep the limit well above
# the largest entry: one chemical's source apportionment, about 14 MB (most
# entries are well under 1 MB).
#
# The full-resolution spatial layers (full_res_layer()) keep their own
# cache: a fixed set of three (~53 MB), so it cannot grow, and evicting them
# would mean re-parsing the water body GeoJSON (~1 s) on a later download.
app_cache <- local({
  mb <- suppressWarnings(as.numeric(env_setting("ECOMIX_CACHE_MB", "512")))
  if (is.na(mb) || mb <= 0) {
    stop("ECOMIX_CACHE_MB must be a positive number, not \"", Sys.getenv("ECOMIX_CACHE_MB"), "\"",
         call. = FALSE)
  }
  cachem::cache_mem(max_size = mb * 1024^2, evict = "lru")
})

# The cached value for `namespace` and the key parts in `...`, from compute()
# on a miss. cachem keys may only contain [a-z0-9], so the key is a hash of
# the namespace and the parts, each converted with as.character() first - the
# same conversion paste() made for the keys this replaced, so 353 and "353"
# still share an entry.
cache_fetch <- function(namespace, ..., compute) {
  key <- rlang::hash(c(namespace, unlist(lapply(list(...), as.character))))
  value <- app_cache$get(key)
  if (cachem::is.key_missing(value)) {
    value <- compute()
    app_cache$set(key, value)
  }
  value
}
