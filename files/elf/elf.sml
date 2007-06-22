
structure Elf =
struct

    (* I use names from the specification in order to make the
       source easier to read along side it. *)

    val magic = "\127ELF"

    exception Elf of string

    type w32 = Word32.word

    fun I x = x
    fun cons a b = a :: b
    val wtoi = Word32.toIntX
    val itow = Word32.fromInt

    datatype oftype =
	ET_NONE
      | ET_REL
      | ET_EXEC
      | ET_DYN
      | ET_CORE
      | ET_UNKNOWN of int

    datatype machtype =
	EM_NONE
      | EM_M32
      | EM_SPARC
      | EM_386
      | EM_68K
      | EM_88K
      | EM_486
      | EM_860
      | EM_MIPS
      | EM_S370
      | EM_MIPS_RS4_BE
      | EM_RS6000
      | EM_PARISC
      | EM_nCUBE
      | EM_VPP500
      | EM_SPARC32PLUS
      | EM_960
      | EM_PPC
      | EM_V800
      | EM_FR20
      | EM_RH32
      | EM_MMA
      | EM_ARM
      | EM_FAKE_ALPHA
      | EM_SH
      | EM_SPARCV9
      | EM_TRICORE
      | EM_ARC
      | EM_H8_300
      | EM_H8_300H
      | EM_H8S
      | EM_H8_500
      | EM_IA_64
      | EM_MIPS_X
      | EM_COLDFIRE
      | EM_68HC12
      | EM_UNKNOWN of int

    datatype 'a section =
	Unparsed of 'a
      | Comment of string

    datatype sectype =
	SHT_NULL
      | SHT_PROGBITS
      | SHT_SYMTAB of int * int
      | SHT_STRTAB
      | SHT_RELA of int * int
      | SHT_HASH of int
      | SHT_DYNAMIC of int
      | SHT_NOTE
      | SHT_NOBITS
      | SHT_REL of int * int
      | SHT_SHLIB
      | SHT_DYNSYM of int * int
      | SHT_UNKNOWN of w32

    fun mustbezero constant 0 0 = constant
      | mustbezero constant _ _ = constant 
         (* raise Elf "sh_link and sh_info must be 0 for this section type"*)

    fun secondzero maker n 0 = maker n
      | secondzero _ _ _ = raise Elf "sh_info must be 0 for this section type"

    fun twoparams maker n m = maker (n, m)

    val sectypes = Vector.fromList 
        [mustbezero SHT_NULL,
         mustbezero SHT_PROGBITS,
         twoparams  SHT_SYMTAB,
         mustbezero SHT_STRTAB,
         twoparams  SHT_RELA,
         secondzero SHT_HASH,
         secondzero SHT_DYNAMIC,
         mustbezero SHT_NOTE,
         mustbezero SHT_NOBITS,
         twoparams  SHT_REL,
         mustbezero SHT_SHLIB,
         twoparams  SHT_DYNSYM]

    val num_sectype = Vector.length sectypes

    datatype ptype = 
	PT_NULL
      | PT_LOAD
      | PT_DYNAMIC
      | PT_INTERP
      | PT_NOTE
      | PT_SHLIB
      | PT_PHDR
      | PT_UNKNOWN of int

    val ptypes = Vector.fromList [PT_NULL, PT_LOAD, PT_DYNAMIC, 
				  PT_INTERP, PT_NOTE, PT_SHLIB, PT_PHDR]

    val num_ptype = Vector.length ptypes

    (* missing from spec, culled from linux elf.h *)
    datatype proflag =
	  PF_R
	| PF_W
        | PF_X

    val proflags = [(PF_X, 1), (PF_W, 2), (PF_R, 4)]

    datatype secflag =
	SHF_EXECINSTR
      | SHF_ALLOC
      | SHF_WRITE

    val secflags = [(SHF_WRITE, 1), (SHF_ALLOC, 2), (SHF_EXECINSTR, 4)]

    (* move to utility library? *)
    fun getflags nil _ = nil
      | getflags ((r,b)::t) n = 
	if (0w0 < Word32.andb (itow b, n)) then r :: getflags t n
	else getflags t n

    fun read (f as {size, seek, pos, char, vec, close}) =
	let
	    (* reads e_ident field (16 bytes) *)

	    val _ = (vec 4) = magic
		orelse raise Elf "Bad magic number -- not an Elf file?"
	    val _ = char () = #"\001"
		orelse raise Elf "Class must be 32-bit"

	    (* decide endianness *)
	    val (r16, r32) = 
		case char () of
		    #"\001" => (Reader.rlw16, Reader.rlw32)
		  | #"\002" => (Reader.rbw16, Reader.rbw32)
		  | _ => raise Elf "Bad endianness"

	    val _ = char () = #"\001"
		orelse raise Elf "Bad version number: ei_version"

	    (* ignore padding *)
	    val _ = vec 9

	    (* read the rest of the header *)
	    val e_type = (wtoi (r16 f)) handle _ => raise Elf "overflow at 3754!"
	    val e_machine = (wtoi (r16 f)) handle _ => raise Elf "overflow at 3832!"
	    (* e_version *)
	    val _ = r32 f = 0w1
		orelse raise Elf "Bad version number: e_version"
	    val e_entry = r32 f
	    val e_phoff = (wtoi (r32 f)) handle _ => raise Elf "overflow at 4030!"
	    val e_shoff = (wtoi (r32 f)) handle _ => raise Elf "overflow at 4106!"
	    val e_flags = r32 f
	    (* size of ELF header *)
	    val e_ehsize = (wtoi (r16 f)) handle _ => raise Elf "overflow at 4238!"
	    val e_phentsize = (wtoi (r16 f)) handle _ => raise Elf "overflow at 4318!"
	    val e_phnum = (wtoi (r16 f)) handle _ => raise Elf "overflow at 4394!"
	    val e_shentsize = (wtoi (r16 f)) handle _ => raise Elf "overflow at 4474!"
	    val e_shnum = (wtoi (r16 f)) handle _ => raise Elf "overflow at 4550!"
	    (* index into shtable of 'section name string table' *)
	    val e_shstrndx = (wtoi (r16 f)) handle _ => raise Elf "overflow at 4690!"

	    val _ = print ("There are " ^ Int.toString e_shnum ^
			   " sections.\n")

	    val sections = get_sections f r16 r32 
		           e_shoff e_shnum e_shentsize e_shstrndx

	    val program = get_program f r16 r32
		          e_phoff e_phnum e_phentsize
	in
	    (sections, program)
	end
    
    and get_program _ _ _ 0 progs _ =
	let in
	    progs = 0 orelse raise Elf "e_phnum must be 0 when e_phoff is 0";
	    NONE
	end
      | get_program (f as {size, seek, pos, char, vec, close})
	            r16 r32 phoff progs 32 =
        let
	    val _ = seek phoff

	    fun get 0 = nil
	      | get n = 
		let

		    val p_type   = (wtoi (r32 f)) handle _ => raise Elf "overflow at 5383!"
		    val p_offset = (wtoi (r32 f)) handle _ => raise Elf "overflow at 5461!"
		    val p_vaddr  = r32 f
		    val p_paddr  = r32 f
		    val p_filesz = (wtoi (r32 f)) handle _ => raise Elf "overflow at 5593!"
		    val p_memsz  = (wtoi (r32 f)) handle _ => raise Elf "overflow at 5671!"
		    val p_flags  = getflags proflags (r32 f)
		    val p_align  = (wtoi (r32 f)) handle _ => raise Elf "overflow at 5796!"
		in 
		    (case if p_type >= num_ptype then PT_UNKNOWN p_type
			  else Vector.sub(ptypes, p_type) of
			      PT_NULL => (* skip *) I
			    | t => 
				  let
				      
				  (* XXX check that p_align is power of two *)
				  in
				      cons 
				      (t, p_offset, p_vaddr, p_paddr, p_filesz, 
				       p_memsz, p_flags, p_align)
				  end)
			 (get (n - 1))
		end
	in
	    SOME (get progs)
	end
      | get_program _ _ _ _ _ _ = raise Elf "e_phentsize should be 32"

    and get_sections _ _ _ 0 secs _ _ = 
	let in
	    secs = 0 orelse raise Elf "e_shnum must be 0 when e_shoff is 0";
	    NONE
	end
      | get_sections (f as {size, seek, pos, char, vec, close}) 
	             r16 r32 shoff secs 40 stringsec =
	let

	    fun get 0 = nil
	      | get n =
		let
		    val sh_name   = (wtoi (r32 f)) handle _ => raise Elf "overflow at 6650!"
		    val sh_type   = (r32 f) handle _ => raise Elf "overflow at 6729!"
		    val sh_flags  = r32 f
		    val sh_addr   = r32 f
		    val sh_offset = (wtoi (r32 f)) handle _ => raise Elf "overflow at 6864!"
		    val sh_size   = (wtoi (r32 f)) handle _ => raise Elf "overflow at 6943!"
		    val sh_link   = (wtoi (r32 f)) handle _ => raise Elf "overflow at 7022!"
		    val sh_info   = (wtoi (r32 f)) handle _ => raise Elf "overflow at 7101!"
		    val sh_addralign = (wtoi (r32 f)) handle _ => raise Elf "overflow at 7183!"
		    val sh_entsize = (wtoi (r32 f)) handle _ => raise Elf "overflow at 7263!"

		    val flist = getflags secflags sh_flags
		    val shtype = if Word32.>= (sh_type, itow num_sectype) then SHT_UNKNOWN sh_type
			         else (Vector.sub(sectypes, wtoi sh_type)) sh_link sh_info
				     
		    (* get the section data as a pair of its beginning and length, if
		       the section exists. (Since sections may be pretty big, we don't
		       read them into memory, though we verify that the offset and
		       size are within the right ranges...) *)
		    val secdata =
			if sh_offset = 0 orelse sh_size = 0 then NONE
			else case shtype of
			    SHT_NULL => NONE
			  | SHT_NOBITS => NONE
			  | _ => let in
				    sh_size > 0 andalso
				    sh_offset > 0 andalso
				    (sh_offset + sh_size) <= size
				       orelse raise Elf "section does not reside in file!";
				    SOME (sh_offset, sh_size)
				 end

		(* XXX check that sh_addralign is a power of 2 *)
		in
		    (sh_name, shtype, flist, sh_addr, secdata, sh_addralign, sh_entsize)
		end :: get (n - 1)

	    fun getzeros 0 = ()
	      | getzeros n = 
		let in
		    char () = #"\000" 
		      orelse raise Elf "first section header table entry must be 40 zeros";
		    getzeros (n - 1)
		end

	    val _ = seek shoff
	    val _ = getzeros 40

	    val ss = get (secs - 1)

	    val _ = print ("stringsec is " ^ Int.toString stringsec  ^ "\n")

	    val strtab = (case List.nth (ss, stringsec - 1) of
			      (_, SHT_STRTAB, _, _, SOME (beg, len), _, _) =>
				  (* XXX Something like Reader.extract might be useful here *)
				  Reader.fromvec (Reader.vecat f beg len)
			    | _ => raise Elf "header has bad sh_strndx: wrong type / no data")
		              handle Subscript => raise Elf "header has bad sh_strndx : index too big"
	    
	    fun fetchname (0, a, b, c, d, e, f) = (NONE, a, b, c, d, e, f)
	      | fetchname (n, a, b, c, d, e, f) = (SOME (Reader.strzat strtab n), a, b, c, d, e, f)


	    val sss = map fetchname ss
	in
	    SOME sss
	end
      | get_sections _ _ _ _ _ _ _ = raise Elf "e_shentsize should be 40"

    fun parse_sec f = Unparsed
    fun parse_pro f x = x

    fun load f =
	let
	    val (sec, pro) = read f
	in
	    (Option.map (List.map (parse_sec f)) sec,
	     Option.map (List.map (parse_pro f)) pro)
	end
end