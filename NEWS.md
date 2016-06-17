# Version 0.19.2 (2016-06-17)

* Bunch of changes from reviews

* `item_get_parent` now returns `sbitem`, not just ID

* fixed probablem with `query_sb_doi`

* `query_sb_spatial` no longer has awkard bounding box specification, 
just uses lat/long arrays and determines box from those

* `sb_ping` returns boolean TRUE/FALSE for success/fail

* `set_endpoint` no longer includes verbose message and properly uses `match.arg`

* Lots of documentation updates and new demos

# Version 0.18.5 (2016-06-01)

* Fixed issue with `query_sb_datatype`

* New and improved `item_get_wfs`. Better performance and 
no longer requires hard-to-install external dependencies.

# Version 0.18.0 (2016-04-10)

* Improved version of `query_sb()` now requests useful metadata so
`sbitem` list has key metadata

* `query_item_identifier()` now returns an `sbitem` list instead of a 
data.frame. Also, `*_item_identifier()` funcitons now have unified 
parameter order


# Version 0.17.0 (2016-04-10)

* On `item_replace_files()` changed default on `all` flag to FALSE
so it doesn't delete files by default.

* Added `item_rename_files()` to enable user to easily 
rename files attached to items directly. 


