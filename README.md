# Delphi.Parallel.FileScanner
Getting file list in parallel

Currently more than less One Trick Poney. Getting files with list of extensions within list of root directories. You can exclude fiels with with prefix and suffix.

It scans given list of floders in parallel. If you can divide it those smaller pieces, then it startts to pay off (E.g not much use, if to scan only one large folder). 

TODO:
  - Code needs further drefactoring
  - Filtering should be done while searching,
    - At least for prefixes, why to start scan if folder is ignored
  - ...
