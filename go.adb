pragma Profile(Restricted);
pragma Restrictions (No_Abort_Statements);
pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);
pragma Restrictions (No_Exceptions);
pragma Restrictions (No_Direct_Boolean_Operators);

with Ada.Text_IO;
use Ada.Text_IO;

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

with Ada.Real_Time;
use Ada.Real_Time;

with Interfaces;
use Interfaces;

with System.Storage_Elements;
use System.Storage_Elements;

with System.Address_To_Access_Conversions;

procedure Go is
    
    generic
        type Element_Type is private;
    procedure Swap(Left, Right : in out Element_Type)
        with Inline;

    procedure Swap(Left, Right : in out Element_Type) is 
        Temporary : constant Element_Type := Left;
    begin
        Left := Right;
        Right := Temporary;
    end Swap;



    generic
        type Element_Type is private;
        with function "+" (Left, Right : Element_Type) return Element_Type is <>;
    procedure Generic_Add(X : in out Element_Type; Y : in Element_Type)
        with Inline;

    procedure Generic_Add(X : in out Element_Type; Y : in Element_Type) is begin
        X := X + Y;
    end Generic_Add;

    procedure Add is new Generic_Add(Element_Type => Integer);

    package Int_IO is new Integer_IO(Integer);

    --------------------------------------------------------------------------
    -- Random Signature Package

    generic
        type Random_Type;

        with function Next(
            Random : in out Random_Type;
            Size : in Unsigned_32
        ) return Unsigned_32 is <>;

    package Signature_Random is end;

    --------------------------------------------------------------------------
    -- XorShiftPlus Packages implementing Random Signature

    package XorShiftPlus is
        type Random_Type is limited private;

        function Init(X, Y : in Unsigned_64) return Random_Type
            with Inline;

        function Next(
            Random : in out Random_Type;
            Size : in Unsigned_32
        ) return Unsigned_32
            with Inline;

        package Signature is new Signature_Random(
            Random_Type => Random_Type
        );

    private
        type Random_Type is limited record
            S0 : Unsigned_64 := 123456789362436069;
            S1 : Unsigned_64 := 52128862988675123;
        end record;
    end XorShiftPlus;

    package body XorShiftPlus is
        function Init(X, Y : in Unsigned_64) return Random_Type is
        begin
            return (S0 => X, S1 => Y);
        end Init;

        function Next(
            Random : in out Random_Type;
            Size : in Unsigned_32
        ) return Unsigned_32 is
            S1 : Unsigned_64 := Random.S0;
            S0 : constant Unsigned_64 := Random.S1;
        begin
            S1 := S1 xor (Shift_Left(S1, 23));
            Random.S0 := S0;
            S1 := S1 xor S0 xor Shift_Right(S1, 17) xor Shift_Right(S0, 26);
            Random.S1 := S1;
            return Unsigned_32(Shift_Right((((S1 + S0) and 16#ffffffff#) * Unsigned_64(Size)), 32));
        end Next;

    end XorShiftPlus;

    --------------------------------------------------------------------------
    -- Disjointset Signature Package

    generic
        type Element_Type;
        type Index;

        type Set_Type; 
        type Cursor;

        with function None return Cursor is <>;
        with function Find(Node : in Cursor) return Cursor is <>;
        with procedure Makeset(Node : in Cursor) is <>;
        with function Link(C1, C2 : in Cursor) return Cursor is <>;
        with procedure Link_Circular(Left, Right : in Cursor) is <>;

        with function Succ(Node : in Cursor) return Cursor is <>;
        with procedure Set_Succ(Node, Succ : in Cursor) is <>;

        with function First(Set : access Set_Type) return Cursor is <>;
        with function Element(Node : in Cursor) return access Element_Type is <>;
        with function Index_Of(Set : access Set_Type; Node : in Cursor) return Index is <>;
        with procedure Next(Node : in out Cursor) is <>;

        with function Step return Integer is <>;
        with function "+" (Left : in Cursor; Right : in Integer) return Cursor is <>;
        with function "-" (Left : in Cursor; Right : in Integer) return Cursor is <>;
    package Signature_Disjointsets is end;

    --------------------------------------------------------------------------
    -- Disjoint_Eager Packages implementing Disjointset Signature

    generic
        type Element_Type is limited private;
        type Index is range <>;
    package Eager_Disjointsets is
        type Set_Type is limited private;
        type Node_Type is limited private;
        type Cursor is access all Node_Type;

        function None return Cursor
            with Inline;

        function Find(Node : in Cursor) return Cursor
            with Inline;

        procedure Makeset(Node : in Cursor)
            with Inline;

        function Link(C1, C2 : in Cursor) return Cursor
            with Inline;

        procedure Link_Circular(Left, Right : in Cursor)
            with Inline;

        function Succ(Node : in Cursor) return Cursor
            with Inline;

        procedure Set_Succ(Node, Succ : in Cursor)
            with Inline;

        function First(Set : access Set_Type) return Cursor
            with Inline;

        function Element(Node : in Cursor) return access Element_Type
            with Inline;

        function Index_Of(Set : access Set_Type; Node : in Cursor) return Index
            with Inline;

        procedure Next(Node : in out Cursor)
            with Inline;

        function Step return Integer
            with Inline;

        function "+" (Left : in Cursor; Right : in Integer) return Cursor
            with Inline;

        function "-" (Left : in Cursor; Right : in Integer) return Cursor
            with Inline;

        package Signature is new Signature_Disjointsets(
            Element_Type => Element_Type,
            Set_Type => Set_Type,
            Index => Index,
            Cursor => Cursor
        );

    private
        type Node_Type is limited record
            Canonical : Cursor;
            Successor : Cursor;
            Size : Unsigned_32;
            Value : aliased Element_Type;
        end record;
        type Set_Type is array (Index) of aliased Node_Type;

        package Node_Address is new System.Address_To_Access_Conversions(Node_Type);

        Node_Size : constant Storage_Offset := Node_Type'Object_Size
            / System.Storage_Elements.Storage_Element'Size;
    end Eager_Disjointsets;



    package body Eager_Disjointsets is

        function To_Access(A : System.Address) return access Node_Type
            with Inline
        is begin
            return Node_Address.To_Pointer(A);
        end To_Access;

        function To_Address(C : access Node_Type) return System.Address
            with Inline
        is begin
            return Node_Address.To_Address(Node_Address.Object_Pointer(C));
        end To_Address;

        function None return Cursor is
        begin
            return null;
        end None;

        procedure Makeset(
            Node : in Cursor
        ) is begin
            Node.Canonical := Node;
            Node.Size := 1;
        end Makeset;

        function Find(
            Node : in Cursor
        ) return Cursor is begin
            return Node.Canonical;
        end Find;

        procedure Swap_Cursors is new Swap(Cursor);

        procedure Link_Circular(Left, Right : Cursor) is begin
             Swap_Cursors(Left.Successor, Right.Successor);
        end Link_Circular;

        procedure Link_Head(Head, Tail : in Cursor) 
            with Inline;

        procedure Link_Head(Head, Tail : in Cursor) is 
            C : Cursor := Tail;
        begin
            Link_Loop : loop
                C.Canonical := Head;
                C := C.Successor;
                exit Link_Loop when C = Tail;
            end loop Link_Loop;

            Head.Size := Head.Size + Tail.Size;
        end Link_Head;

        function Link(C1, C2 : in Cursor) return Cursor is begin
            if C1.Size < C2.Size then
                Link_Head(C2, C1);
                return C2;
            else
                Link_Head(C1, C2);
                return C1;
            end if;
        end Link;

        function Succ(
            Node : in Cursor
        ) return Cursor is begin
            return Node.Successor;
        end Succ;

        procedure Set_Succ(
            Node, Succ : in Cursor
        ) is begin
            Node.Successor := Succ;
        end Set_Succ;

        function First(
            Set : access Set_Type
        ) return Cursor is begin
            return Set(Set_Type'First)'Access;
        end First;

        function Element(
            Node : in Cursor
        ) return access Element_Type is begin
            return Node.Value'Access;
        end Element;

        function Index_Of(
            Set : access Set_Type;
            Node : in Cursor
        ) return Index is begin
            return Index((To_Address(Node) - To_Address(First(Set))) / Node_Size);
        end Index_Of;

        procedure Next(
            Node : in out Cursor
        ) is begin
            Node := To_Access(To_Address(Node) + Node_Size);
        end Next;

        function Step return Integer is
        begin
            return Integer(Node_Size);
        end Step;

        function "+" (Left : in Cursor; Right : in Integer) return Cursor is
        begin
            return To_Access(To_Address(Left) + Storage_Offset(Right));
        end "+";

        function "-" (Left : in Cursor; Right : in Integer) return Cursor is
        begin
            return To_Access(To_Address(Left) - Storage_Offset(Right));
        end "-";
    begin
        Put_Line("region size:" & Storage_Offset'Image(Node_Size) & "B");
    end Eager_Disjointsets;

    --------------------------------------------------------------------------
    -- Sack Signaure Package

    generic
        type Sack;
        type Element;
        type Index;
        type Cursor;

        with function None return Cursor is <>;
        with function Has_Element(C : Cursor) return Boolean is <>;
        with function Size(S : access Sack) return Integer is <>;
        with function Get(S : access Sack; I : in Index) return Cursor is <>;
        with function Deref(C : in Cursor) return Element is <>;
        with function First(S : access Sack) return Cursor is <>;
        with function Last(S : access Sack) return Cursor is <>;
        with function Index_Of(S: access Sack; C : in Cursor) return Index is <>;
        with function Step return Integer is <>;
        with function "+" (C : in Cursor; I : in Integer) return Cursor is <>;
        with function "-" (C : in Cursor; I : in Integer) return Cursor is <>;

        with procedure Reset(S : access Sack) is <>;
        with procedure Add(S : access Sack; E : in Element) is <>;
        with procedure Remove(S : access Sack; C: in Cursor) is <>;
        with procedure Next(C : in out Cursor) is <>;
    package Signature_Sack is end;

    --------------------------------------------------------------------------
    -- Array_Sacks Packages implementing Sack Signature

    generic
        type Element is private;
        type Index is range <>;
    package Array_Sacks is
        type Sack is limited private;
        type Cursor is access all Element;

        function None return Cursor with Inline;
        function Has_Element(A : Cursor) return Boolean with Inline;
        function Size(S: access Sack) return Integer with Inline;
        function Get(S : access Sack; I : in Index) return Cursor with Inline;
        function Deref(A : in Cursor) return Element with Inline;
        function First(S : access Sack) return Cursor with Inline;
        function Last(S : access Sack) return Cursor with Inline;
        function Index_Of(S : access Sack; A : in Cursor) return Index with Inline;
        function Step return Integer with Inline;
        function "+" (A : in Cursor; I : in Integer) return Cursor with Inline;
        function "-" (A : in Cursor; I : in Integer) return Cursor with Inline;

        procedure Reset(S : access Sack) with Inline;
        procedure Add(S : access Sack; E : in Element) with Inline;
        procedure Remove(S : access Sack; A : in Cursor) with Inline;
        procedure Next(A : in out Cursor) with Inline;

        package Signature is new Signature_Sack(
            Element => Element,
            Index => Index,
            Sack => Sack,
            Cursor => Cursor
        );

    private
        type Element_Array is array (Index) of aliased Element;
        type Sack is limited record
            Used : Integer := Integer(Index'First);
            Elements : Element_Array;
        end record;
    end Array_Sacks;



    package body Array_Sacks is
        package Element_Address is new System.Address_To_Access_Conversions(Element);

        Element_Size : constant Storage_Offset := Element'Object_Size
            / System.Storage_Elements.Storage_Element'Size;

        function To_Cursor(A : System.Address) return Cursor with Inline is begin
            return Cursor(Element_Address.To_Pointer(A));
        end To_Cursor;

        function To_Address(A : Cursor) return System.Address with Inline is begin
            return Element_Address.To_Address(Element_Address.Object_Pointer(A));
        end To_Address;

        function None return Cursor is
        begin
            return null;
        end None;

        function Has_Element(A : in Cursor) return Boolean is begin
            return A = null;
        end Has_Element;

        function Size(S : access Sack) return Integer is begin
            return S.Used;
        end Size;

        function Deref(A : in Cursor) return Element is begin
            return A.all;
        end Deref;

        procedure Reset(S : access Sack) is begin
            S.Used := Integer(Index'First);
        end Reset;

        procedure Add(S : access Sack; E : in Element) is
            pragma Precondition(S.Used >= Integer(Index'First));
            pragma Precondition(S.Used <= Integer(Index'Last));
        begin
            S.Elements(Index(S.Used)) := E;
            S.Used := Integer(Index'Succ(Index(S.Used)));
        end Add;

        procedure Remove(S : access Sack; A : in Cursor) is begin
            S.Used := Integer(Index'Pred(Index(S.Used)));
            A.all := S.Elements(Index(S.Used));
        end Remove;

        function Get(S : access Sack; I : in Index) return Cursor is begin
            return S.Elements(I)'Access;
        end Get;

        function First(S : access Sack) return Cursor is begin
            return S.Elements(Index'First)'Access;
        end First;

        function Last(S : access Sack) return Cursor is begin
            return S.Elements(Index(S.Used))'Access;
        end Last;

        procedure Next(A : in out Cursor) is begin
            A := To_Cursor(To_Address(A) + Element_Size);
        end Next;

        function Index_Of(S : access Sack; A : in Cursor) return Index is begin
            return Index((To_Address(A) - To_Address(First(S))) / Element_Size);
        end Index_Of;

        function Step return Integer is begin
            return Integer(Element_Size);
        end Step;

        function "+" (A : in Cursor; I : in Integer) return Cursor is begin
            return To_Cursor(To_Address(A) + Storage_Offset(I));
        end "+";

        function "-" (A : in Cursor; I : in Integer) return Cursor is begin
            return To_Cursor(To_Address(A) - Storage_Offset(I));
        end "-";
    end Array_Sacks;

    --------------------------------------------------------------------------
 
    package Colours is
        type Colour_Type is (Black, White, Red, None);
        for Colour_Type use (Black => 0, White => 1, Red => 2, None => 3);

        function Opposite(Colour : Colour_Type) return Colour_Type
            with Inline;
    end Colours;

    package body Colours is
        function Opposite(Colour : Colour_Type) return Colour_Type is begin
            return Colour_Type'Val(1 - Colour_Type'Pos(Colour));
        end Opposite;
    end Colours;

    --------------------------------------------------------------------------

    package Nodes is 
        use Colours;

        type Neighbour_Count is new Unsigned_8;
        type Neighbour_Count_Access is not null access all Neighbour_Count;
        type Neighbours_Array is array (Colour_Type) of aliased Neighbour_Count;
        type Neighbours_Access is not null access all Neighbours_Array;
        
        type Node_Type is limited record 
            Pseudo_Liberties : Integer;
            Colour : Colour_Type;
            Neighbours : aliased Neighbours_Array;
        end record;

        type Node_Access is not null access all Node_Type;

        procedure Add is new Generic_Add(Element_Type => Neighbour_Count)
            with Inline;

        function Get(
            N : in Neighbours_Access;
            C : in Colour_Type
        ) return Neighbour_Count_Access
            with Inline;
    end Nodes;

    package body Nodes is
        function Get(
            N : in Neighbours_Access;
            C : in Colour_Type
        ) return Neighbour_Count_Access is 
        begin
            return N(C)'Access;
        end Get;
    end Nodes;

    --------------------------------------------------------------------------
    -- note: board rows and cols include borders.

    generic 
        Width : Integer := 9;
        Height : Integer := 9;
    package Board_Config is
        Cols : constant Integer := Width + 2;
        Rows : constant Integer := Height + 2;
        Action_Area : constant Integer := Width * Height;
        Total_Area : constant Integer := Cols * Rows;
        Max_Moves : constant Integer := 3 * Action_Area;
        subtype Col is Integer range 1 .. Cols;
        subtype Row is Integer range 1 .. Rows;
        subtype Region_Index is Integer range 0 .. Total_Area - 1;
        subtype Moves_Index is Integer range 0 .. Action_Area - 1;
    end Board_Config;
    
    generic
        type Random_type is limited private;

        type Set_Type is limited private;
        type Set_Cursor is private;

        type Sack_Type is limited private;
        type Sack_Cursor is private;

        with package Config is new Board_Config(<>);
        with package Regions is new Signature_Disjointsets(
            Element_Type => Nodes.Node_Type,
            Index => Config.Region_Index,
            Set_Type => Set_Type,
            Cursor => Set_Cursor,
            others => <>
        );
        with package Moves is new Signature_Sack(
            Element => Set_Cursor,
            Index => Config.Moves_Index,
            Sack => Sack_Type,
            Cursor => Sack_Cursor,
            others => <>
        );
        with package Random is new Signature_Random(
            Random_Type => Random_Type,
            others => <>
        );
    package Boards is
        type Board_Type is limited private;
        type Board_Access is not null access all Board_Type;
        procedure Reset(
            Board : in out Board_Type
        );

        procedure Show(
            Board : in out Board_Type
        );

        procedure Benchmark_Monte_Carlo(
            Board : in out Board_Type;
            Rand : in out Random_Type;
            Reps : in Integer
        );

        procedure Show_Actions(
            Board : in out Board_Type
        );

    private
        use Config;
        use Colours;
        use Regions;
        use Moves;
        use Nodes;

        type Point_Array is array (0 .. 1) of Integer;
        type Board_Type is limited record
            Regions : aliased Set_Type;
            Moves : aliased Sack_Type;
            Ko : Set_Cursor;
            Points : Point_Array;
            Passes : Integer;
            Plies : Integer;
            Player : Colour_Type;
        end record;

        Board_Size : constant Storage_Offset := Board_Type'Object_Size
            / System.Storage_Elements.Storage_Element'Size;

        Region_DX : constant Integer := Regions.Step;
        Region_DY : constant Integer := Region_DX * Config.Cols;
    end Boards;

    package body Boards is
        procedure Show_Action(
            Board : in out Board_Type;
            C : in Set_Cursor
        ) is
            subtype Heading_Index is Integer range Config.Col'First .. Integer'Last;
            H : constant array (Heading_Index range <>) of Character :=
                "ABCDEFGHJKLMNOPQRSTUVWXYZ";
            K : constant Config.Region_Index := Regions.Index_Of(Board.Regions'Access, C);
            X : constant Integer := K mod Config.Cols;
            Y : constant Integer := K / Config.Cols;
        begin
            Put("[" & H(X));
            Int_IO.Put(Config.Cols - Y - 1, 0);
            Put("]");
        end Show_Action;

        procedure Show_Actions(Board : in out Board_Type) is
            M : constant access Sack_Type := Board.Moves'Access;
        begin
            Int_IO.Put(Size(M), 0);
            Put(" : ");
            for J in Config.Moves_Index'First .. Size(M) - 1 loop
                Show_Action(Board, Deref(Moves.Get(M, J)));
            end loop;
            New_Line;
        end Show_Actions;

        procedure Reset(
            c : in Set_Cursor;
            Colour : in Colour_Type;
            Pseudo_Liberties : in Integer
        )
            with Inline;
                
        procedure Reset(
            C : in Set_Cursor;
            Colour : in Colour_Type;
            Pseudo_Liberties : in Integer
        ) is
            Neighbours : constant Neighbour_Count :=
                Neighbour_Count(4 - Pseudo_Liberties);
        begin
            Makeset(C);
            Set_Succ(C, C);
            declare
                N : constant Node_Access := Regions.Element(C);
                M : constant Neighbours_Access := N.Neighbours'Access;
            begin
                N.Pseudo_Liberties := Pseudo_Liberties;
                N.Colour := Colour;
                Get(M, Black).all := Neighbours;
                Get(M, White).all := Neighbours;
                Get(M, Red).all := Neighbours;
            end;
        end Reset;

        procedure Reset(Board : in out Board_Type) is
            M : constant access Sack_Type := Board.Moves'Access;
            C : Set_Cursor := First(Board.Regions'Access);
        begin
            Reset(M);

            Reset(C, Red, 0); 
            Next(C);
            for X in 3 .. Config.Cols loop
                Reset(C, Red, 1);
                Next(C);
            end loop;
            Reset(C, Red, 0); 
            Next(C);
    
            Reset(C, Red, 1);
            Next(C);
            Reset(C, None, 2);
            Add(M, C);
            Next(C);
            for X in 5 .. Config.Cols loop
                Reset(C, None, 3);  
                Add(M, C);
                Next(C);
            end loop;
            Reset(C, None, 2);
            Add(M, C);
            Next(C);
            Reset(C, Red, 1);
            Next(C);

            for Y in 5 .. Config.Rows loop
                Reset(C, Red, 1);
                Next(C);
                Reset(C, None, 3);
                Add(M, C);
                Next(C);
                for X in 5 .. Config.Cols loop
                    Reset(C, None, 4);
                    Add(M, C);
                    Next(C);
                end loop;
                Reset(C, None, 3);
                Add(M, C);
                Next(C);
                Reset(C, Red, 1);
                Next(C);
            end loop;

            Reset(C, Red, 1);
            Next(C);
            Reset(C, None, 2);
            Add(M, C);
            Next(C);
            for X in 5 .. Config.Cols loop
                Reset(C, None, 3);  
                Add(M, C);
                Next(C);
            end loop;
            Reset(C, None, 2);
            Add(M, C);
            Next(C);
            Reset(C, Red, 1);
            Next(C);

            Reset(C, Red, 0); 
            Next(C);
            for X in 3 .. Config.Cols loop
                Reset(C, Red, 1);
                Next(C);
            end loop;
            Reset(C, Red, 0); 
            
            Board.Ko := None;
            Board.Points := (0, 0);
            Board.Passes := 0;
            Board.Plies := 0;
            Board.Player := Black;
        end Reset;

        procedure Show(Board : in out Board_Type) is
            Esc: Character renames Ada.Characters.Latin_1.ESC;
            subtype Heading_Index is Integer range Config.Col'First .. Integer'Last;
            H : constant array (Heading_Index range <>) of Character :=
                "ABCDEFGHJKLMNOPQRSTUVWXYZ";
            C : Set_Cursor := First(Board.Regions'Access);
        begin
            Put("    ");
            for X in Config.Col'First .. Config.Col'Last - 2 loop
                Put(" " & H(X));
            end loop;
            New_Line;
            for Y in Row'First .. Row'Last loop
                if (Config.Row'Last > Y) and then (Y > 1) then
                    Int_IO.Put(Integer(Config.Row'Last) - Integer(Y), 2);
                else
                    Put("  ");
                end if;

                for X in Config.Col'First .. Config.Col'Last loop
                    declare
                        N : constant Node_Access := Regions.Element(Find(C));
                    begin
                        case N.Colour is
                            when Black => Put(Esc & "[7m");
                            when Red => Put(Esc & "[41;30;2m");
                            when None => Put(Esc & "[47;2m");
                            when White => null;
                        end case;
                        Int_IO.Put(N.Pseudo_Liberties, 2);
                        Put(Esc & "[0m");
                    end;
                    Next(C);
                end loop;
                New_Line;
            end loop;
        end Show;

        procedure Remove(
            C : in Set_Cursor;
            Colour : in Colour_Type
        )
            with Inline
        is
            N : constant Neighbour_Count_Access :=
                Get(Regions.Element(C).Neighbours'Access, Colour);
            P : constant Node_Access := Regions.Element(Find(C));
        begin
            N.all := Neighbour_Count'Pred(N.all);
            P.Pseudo_Liberties := Integer'Succ(P.Pseudo_Liberties);
        end Remove;

        function Check_Suicide(
            Node : in Node_Access;
            Colour : in Colour_Type
        ) return Boolean
            with Inline
        is begin
            return (Node.Colour = Red)
                or else ((Node.Colour = Colour) = (Node.Pseudo_Liberties = 0));
        end Check_Suicide;

        function Is_Ko(
            Board : in Board_Type;
            C : in Set_Cursor
        ) return Boolean
            with Inline
        is begin
            return C = Board.Ko;
        end Is_Ko;

        function Is_Suicide(
            Board : in Board_Type;
            C : in Set_Cursor
        ) return Boolean
            with Inline
        is begin    
            if Regions.Element(C).Pseudo_Liberties /= 0 then
                return false;
            end if;
        
            declare
                NA : constant Node_Access := Regions.Element(Find(C - Region_DY));
                WA : constant Node_Access := Regions.Element(Find(C - Region_DX));
                EA : constant Node_Access := Regions.Element(Find(C + Region_DX));
                SA : constant Node_Access := Regions.Element(Find(C + Region_DY));
    
                NP : constant Integer := NA.Pseudo_Liberties;
                WP : constant Integer := WA.Pseudo_Liberties;
                EP : constant Integer := EA.Pseudo_Liberties;
                SP : constant Integer := SA.Pseudo_Liberties;
            begin
                NA.Pseudo_Liberties := Integer'Pred(NA.Pseudo_Liberties);
                WA.Pseudo_Liberties := Integer'Pred(WA.Pseudo_Liberties);
                EA.Pseudo_Liberties := Integer'Pred(EA.Pseudo_Liberties);
                SA.Pseudo_Liberties := Integer'Pred(SA.Pseudo_Liberties);

                declare
                    Suicide : constant Boolean := Check_Suicide(NA, Board.Player)
                        and then Check_Suicide(WA, Board.Player) 
                        and then Check_Suicide(EA, Board.Player) 
                        and then Check_Suicide(SA, Board.Player);
                begin
                    NA.Pseudo_Liberties := NP;
                    WA.Pseudo_Liberties := WP;
                    EA.Pseudo_Liberties := EP;
                    SA.Pseudo_Liberties := SP;

                    return Suicide;
                end;
            end;
        end Is_Suicide;

        function Is_Eyelike(
            Board : in Board_Type;
            C : in Set_Cursor
        ) return boolean is
            N : constant Node_Access := Regions.Element(C);
            M : constant Neighbours_Access := N.Neighbours'Access;
        begin
            if (N.Pseudo_Liberties /= 0)
                or else (Get(M, Board.Player).all /= 4)
            then
                return false;
            end if;

            declare
                Op_Colour : constant Colour_Type := Opposite(Board.Player);
                NWSE : constant Integer := Region_DY + Region_DX;
                NESW : constant Integer := Region_DY - Region_DX;
            begin
                return Boolean'Pos(Regions.Element(C - NWSE).Colour = Op_Colour)
                    + Boolean'Pos(Regions.Element(C - NESW).Colour = Op_Colour)
                    + Boolean'Pos(Regions.Element(C + NESW).Colour = Op_Colour)
                    + Boolean'Pos(Regions.Element(C + NWSE).Colour = Op_Colour)
                    < 1 + Boolean'Pos(Get(M, Red).all < 1);
            end;
        end Is_Eyelike;
        pragma Inline(Is_Eyelike);

        procedure Capture(
            Board : in out Board_Type;
            C : in Set_Cursor;
            Colour : in Colour_Type
        ) is
            M : constant access Sack_Type := Board.Moves'Access;
            D : Set_Cursor := C;
            N : Node_Access := Regions.Element(C);
        begin
            pragma Debug(Put("Capture: "));
            pragma Debug(Show_Action(Board, C));
            pragma Debug(Ada.Text_IO.New_Line);
            
            Pass1 : loop
                N.Colour := None;
                N.Pseudo_Liberties := 0;
                Makeset(D);
                Add(M, D);
                Add(Board.Points(Colour_Type'Pos(Colour)), -1);
                D := Succ(D);
                N := Regions.Element(D);
                exit Pass1 when D = C;
            end loop Pass1;

            Pass2 : loop
                Remove(D - Region_DY, Colour);
                Remove(D - Region_DX, Colour);
                Remove(D + Region_DX, Colour);
                Remove(D + Region_DY, Colour);

                declare
                    E : constant Set_Cursor := Succ(D);
                begin
                    Set_Succ(D, D);
                    D := E;
                end;
                exit Pass2 when D = C;
            end loop Pass2;
        end Capture;
        pragma Inline(Capture);

        procedure Update_Neighbour_Ko(
            Board : in out Board_Type;
            D : in Set_Cursor;
            My_Colour : in Colour_Type;
            Ko : in out Set_Cursor
        ) is
            E : constant Set_Cursor := Find(D);
            Root : constant Node_Access := Regions.Element(E);
        begin
            Add(Get(Regions.Element(D).Neighbours'Access, My_Colour).all, 1);
            Add(Root.Pseudo_Liberties, -1);
            if (Root.Colour /= Red) and then (Root.Pseudo_Liberties = 0) then
                Capture(Board, E, Root.Colour);
                Ko := E;
            end if;
        end Update_Neighbour_Ko;
        pragma Inline(Update_Neighbour_Ko);

        procedure Update_Neighbour_Hd(
            Board : in out Board_Type;
            D : in Set_Cursor;
            My_Colour : in Colour_Type;
            Op_Colour : in Colour_Type;
            Hd : in out Set_Cursor
        ) is
            E : constant Set_Cursor := Find(D);
            Root : constant Node_Access := Regions.Element(E);
        begin
            Add(Get(Regions.Element(D).Neighbours'Access, My_Colour).all, 1);
            Add(Root.Pseudo_Liberties, -1);
            if Root.Colour = My_Colour then
                if E /= Hd then
                    declare
                        New_Hd : constant Set_Cursor := Link(E, Hd);
                    begin
                        Link_Circular(Hd, E);
                                    
                        Regions.Element(New_Hd).Pseudo_Liberties :=
                            Regions.Element(Hd).Pseudo_Liberties + Root.Pseudo_Liberties;
                        Hd := New_Hd;
                    end;
                end if;
            elsif (Root.Colour = Op_Colour) and then (Root.Pseudo_Liberties = 0) then
                Capture(Board, E, Root.Colour);
            end if;
        end Update_Neighbour_Hd;
        pragma Inline(Update_Neighbour_Hd);

        procedure Result(
            Board : in out Board_Type;
            Action : in Sack_Cursor
        ) is begin

        if Has_Element(Action) then
            Board.Player := Opposite(Board.Player);
            Add(Board.Passes, 1);
            Add(Board.Plies, 1);
            return;
        end if;

        declare
            M : constant access Sack_Type := Board.Moves'Access;
            C : constant Set_Cursor := Deref(Action);
            Node : constant Node_Access := Regions.Element(C);
            My_Colour : constant Colour_Type := Board.Player;
            Op_Colour : constant Colour_Type := Opposite(My_Colour);
            N : constant Set_Cursor := C - Region_DY;
            W : constant Set_Cursor := C - Region_DX;
            E : constant Set_Cursor := C + Region_DX;
            S : constant Set_Cursor := C + Region_DY;
        begin

            Remove(M, Action);
            Node.Colour := My_Colour;
            Add(Board.Points(Colour_Type'Pos(My_Colour)), 1);

            if Get(Node.Neighbours'Access, Op_Colour).all = 4 then
                declare
                    Single_Capture : constant Integer := Size(M) + 1;
                    Ko : Set_Cursor := None;
                begin
                    Update_Neighbour_Ko(Board, N, My_Colour, Ko);
                    Update_Neighbour_Ko(Board, W, My_Colour, Ko);
                    Update_Neighbour_Ko(Board, E, My_Colour, Ko);
                    Update_Neighbour_Ko(Board, S, My_Colour, Ko);
                    if Single_Capture = Size(M) then
                        Board.Ko := Ko;
                    else 
                        Board.Ko := None;
                    end if;
                end;
            else
                declare
                    Hd : Set_Cursor := C;
                begin
                    Update_Neighbour_Hd(Board, N, My_Colour, Op_Colour, Hd);
                    Update_Neighbour_Hd(Board, W, My_Colour, Op_Colour, Hd);
                    Update_Neighbour_Hd(Board, E, My_Colour, Op_Colour, Hd);
                    Update_Neighbour_Hd(Board, S, My_Colour, Op_Colour, Hd);
                    Board.Ko := None;
                end;
            end if;

            Board.Player := Op_Colour;
            Board.Passes := 0;
            Add(Board.Plies, 1);
        end;
        end Result;
        pragma Inline(Result);

        function Not_Terminal(
            Board : in out Board_Type
        ) return Boolean is begin
            return Board.Passes < 2
                and then Board.Plies < Config.Max_Moves;
        end Not_Terminal;
        pragma Inline(Not_Terminal);

        function Utility(
            Board : in out Board_Type;
            My_Colour : in Colour_Type
        ) return Integer is
            M : constant access Sack_Type := Board.Moves'Access;
            Op_Colour : constant Colour_Type := Opposite(My_Colour);
            S : Integer := Board.Points(Colour_Type'Pos(My_Colour))
                - Board.Points(Colour_Type'Pos(Op_Colour));
        begin
            for J in Config.Moves_Index'First .. Size(M) - 1 loop
                declare
                    N : constant Neighbours_Access := Regions.Element(Deref(Get(M, J))).Neighbours'Access;
                begin
                    if Get(N, My_Colour).all = 4 then
                        S := S + 1;
                    elsif Get(N, Op_Colour).all = 4 then
                        S := S - 1;
                    end if;
                end;
            end loop;

            return S;
        end Utility;

        function Is_Illegal(
            Board : in Board_Type;
            C : in Set_Cursor
        ) return Boolean is begin
            return Is_Eyelike(Board, C)
                or else Is_Suicide(Board, C)
                or else Is_Ko(Board, C);
        end Is_Illegal;
        pragma Inline(Is_Illegal);

        function Genmove(
            Board : in out Board_Type;
            Rand : in out Random_Type
        ) return Sack_Cursor is
            M : constant access Sack_Type := Board.Moves'Access;
            --  C : Sack_Cursor := Get(M, Config.Moves_Index(Random.Next(Rand,
            --          Unsigned_32(Size(M)) - Unsigned_32(Config.Moves_Index'First)
            --      ) + Unsigned_32(Config.Moves_Index'First)
            --  ));
            C : Sack_Cursor := Get(M, Config.Moves_Index(
                Random.Next(Rand, Unsigned_32(Size(M)))
            ));
            D : constant Sack_Cursor := C;
            E : constant Sack_Cursor := Last(M);
        begin
            pragma Debug(Put("->"));
            while Is_Illegal(Board, Deref(C)) loop
                pragma Debug(Show_Action(Board, Deref(C)));
                Next(C);
                if C = E then
                    pragma Debug(Put("/"));
                    C := Moves.First(M);
                end if;
                if C = D then
                    pragma Debug(Put("[]"));
                    return Moves.None;
                end if;
            end loop;
            pragma Debug(Show_Action(Board, Deref(C)));
            pragma Debug(Ada.Text_IO.New_Line);
            return C;

            --pragma Debug(Put("->"));
            --while C /= E loop
            --      pragma Debug(Show_Action(Board, Deref(C)));
            --      if not Is_Illegal(Board, Deref(C)) then
            --          pragma Debug(Ada.Text_IO.New_Line);
            --        return C;
            --    end if;
            --    Next(C);
            --end loop;

            --C := Moves.First(M);
            --pragma Debug(Put("/"));
            --while C /= D loop
            --  pragma Debug(Show_Action(Board, Deref(C)));
            --  if not Is_Illegal(Board, Deref(C)) then
            --      pragma Debug(Ada.Text_IO.New_Line);
            --          return C;
            --      end if;
            --      Next(C);
            --end loop;

            --pragma Debug(Put("[]"));
            --pragma Debug(Ada.Text_IO.New_Line);
            --return Moves.None;

        end Genmove;
        pragma Inline(Genmove);
    
        procedure Playout(
            Board : in out Board_Type;
            R : in out Random_Type
        ) is begin
            while Not_Terminal(Board) loop
                pragma Debug(Show(Board));
                pragma Debug(Show_Actions(Board));
                declare
                    C : constant Sack_Cursor := Genmove(Board, R);
                begin
                    Result(Board, C);
                end;
            end loop;
        end Playout;

    procedure Benchmark_Monte_Carlo(
        Board : in out Board_Type;
        Rand : in out Random_Type;
        Reps : in Integer
    ) is
        Plies : Integer := 0;
        Games : Integer := 0;
        Black_Wins : Integer := 0;
        White_Wins : Integer := 0;
        T1, T2 : Time;
    begin
        T1 := Clock;
        while Games < Reps loop
            Reset(Board);
            Playout(Board, Rand);
            if Board.Plies < Config.Max_Moves then
                Plies := Plies + Board.Plies;
                Games := Games + 1;
                declare
                    type Score is delta 0.5 range -500.0 .. 500.0;
                    --package Scr_IO is new Fixed_IO(Score);
                    S : constant Score := Score(Utility(Board, Black)) - 6.5;
                begin
                    --  Int_IO.Put(Integer(S), 0);
                    --  Put(" ");
                    if S > 0.0 then
                        Black_Wins := Black_Wins + 1;
                    elsif S < 0.0 then
                        White_Wins := White_Wins + 1;
                    end if;
                end;
            else
                Put_Line("MAX MOVES EXCEEDED");
            end if;
        end loop;
        T2 := Clock;
            
        declare
            package Flt_IO is new Float_IO(Long_Float);
            DT : constant Long_Float := Long_Float(To_Duration(T2 - T1));
        begin
            Int_IO.Put(Plies, 0);
            Put(" plies over ");
            Int_IO.Put(Games, 0);
            Put(" games in ");
            Flt_IO.Put(DT, 0, 3, 0);
            Put(" seconds.");
            New_Line;
            Flt_IO.Put(Long_Float(Plies) / Long_Float(Reps), 0, 3, 0);
            Put(" plies per game, ");
            Flt_IO.Put(Long_Float(Reps) / (1000.0 * DT), 0, 3, 0);
            Put("k games per second.");
            New_Line;
            Flt_IO.Put(100.0 * Long_Float(Black_Wins) /  Long_Float(Reps), 0, 3, 0);
            Put("% black / ");
            Flt_IO.Put(100.0 * Long_Float(White_Wins) /  Long_Float(Reps), 0, 3, 0);
            Put("% white wins.");
            New_Line;
        end;
    end Benchmark_Monte_Carlo;

    begin
        Put_Line("board size:" & Storage_Offset'Image(Board_Size) & "B");
    end Boards;

    --------------------------------------------------------------------------
    -- Configure Boards to use Eager_Disjointsets and Array_Sacks

    package Config is new Board_Config;

    package My_Eager_Disjointsets is new Eager_Disjointsets(
        Element_Type => Nodes.Node_Type,
        Index => Config.Region_Index
    );

    package My_Array_Sacks is new Array_Sacks(
        Element => My_Eager_Disjointsets.Cursor,
        Index => Config.Moves_Index
    );

    package Board is new Boards(
        Config => Config,

        Random => XorShiftPlus.Signature,
        Random_Type => XorShiftPlus.Random_Type,

        Regions => My_Eager_Disjointsets.Signature,
        Set_Type => My_Eager_Disjointsets.Set_Type,
        Set_Cursor => My_Eager_Disjointsets.Cursor,

        Moves => My_Array_Sacks.Signature,
        Sack_Type => My_Array_Sacks.Sack,
        Sack_Cursor =>  My_Array_Sacks.Cursor
    );

    --------------------------------------------------------------------------
    -- Benchmark Procedure using Random Signature 
    
    generic
        type Random_Type is limited private;
        with package Random is new Signature_Random(
            Random_type => Random_Type,
            others => <>);
    procedure Benchmark_Random(R : in out Random_Type; Reps : in Long_Integer);

    procedure Benchmark_Random(R : in out Random_Type; Reps : in Long_Integer) is
        T1, T2 : Time;
        X : Unsigned_32 := 0;
    begin
        T1 := Clock; 
        for I in 1 .. Reps loop
            X := X + Random.Next(R, 400);
        end loop;       
        T2 := Clock;

        declare
            package U32_IO is new Modular_IO(Unsigned_32);
            package Flt_IO is new Float_IO(Long_Float);
            T : constant Long_Float := (Long_Float(Reps) / 1000000.0)
                / Long_Float(To_Duration(T2 - T1));
        begin
            U32_IO.Put(X, 0);
            Put(" ");
            Flt_IO.Put(T, 0, 3, 0);
            Put("M pseudo-random numbers per second.");
            New_Line;
        end;
    end Benchmark_Random;

    --------------------------------------------------------------------------
    -- Benchmark Monte-Carlo Engine
    
    --------------------------------------------------------------------------
    -- Example use of Test with XorShift implementation of Random Signature

    procedure Benchmark_XorShift is new Benchmark_Random(
        Random_Type => XorShiftPlus.Random_Type,
        Random => XorShiftPlus.Signature
    );
    
    --package My_Board is new My_Boards(Config => My_Config);
    --use My_Board;

    B : Board.Board_Type;
    X : XorShiftPlus.Random_Type;
begin
    Board.Reset(B);
    --Board.Show(B'Access);
    --Board.Show_Actions(B'Access);
 
    --Benchmark_XorShift(X, 1000000000);
    Board.Benchmark_Monte_Carlo(B, X, 1000000);

end Go;
