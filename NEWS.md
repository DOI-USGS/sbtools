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


	