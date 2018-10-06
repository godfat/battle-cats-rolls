* Use server-sent event to pull seek result
* Make fetching APK optional as long as the YAML is built already
* Only show the languages which are built, no need to force all of them
* Use browser timezone offset to calculate local time
* Update event data for each 11 hours automatically
* Tracking history (by recording rolls we click)
* Shutting down gracefully
* Verify APK checksum
* An idea to speed up seeking process. We could pre-process the seeds, and
  create 10 rarity patterns saved in files. Say, starting with seed 1,
  the following rarity will be: R, S, R, R, U, R, and so on, we append the
  seed 1 to the file RSRRUR. We repeat this process for all the seeds, ending
  up with tons of files recoding all the seeds of possible patterns. This
  way, we could use the input rarity pattern to find the corresponding files,
  and only search for those seeds. It could be multiple files because the
  input might not have enough rolls. We should record for 10 rolls pattern,
  say RSRRURSRRU, and RSRRURSRRS (only the last one is different). And the
  input could be just 9 rolls like RSRRURSRR, then we should search for
  both files because they both match the same prefix pattern.
