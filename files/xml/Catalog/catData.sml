

structure CatData =
   struct
      datatype CatEntry =
	 E_BASE of Uri.Uri
       | E_DELEGATE of string * Uri.Uri
       | E_EXTEND of Uri.Uri
       | E_MAP of string * Uri.Uri
       | E_REMAP of Uri.Uri * Uri.Uri
	 
      type Catalog = Uri.Uri * CatEntry list
   end
