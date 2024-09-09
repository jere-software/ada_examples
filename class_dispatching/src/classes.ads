package Classes is

   type Great_Grand_Parent is tagged null record;
   procedure Print(Self : Great_Grand_Parent);
   procedure Print_With_Redispatch(Self : Great_Grand_Parent);
   procedure Print_As_Great_Grand_Parent(Self : Great_Grand_Parent'Class);
   procedure Print_As_Latest_Descendent(Self : Great_Grand_Parent'Class);

   type Grand_Parent is new Great_Grand_Parent with null record;
   overriding procedure Print(Self : Grand_Parent);

   type Parent is new Grand_Parent with null record;
   overriding procedure Print(Self : Parent);

   type Child is new Parent with null record;
   overriding procedure Print(Self : Child);

   type Uncle is new Grand_Parent with null record;

   type Aunt is new Grand_Parent with null record;
   overriding procedure Print(Self : Aunt);

   type Cousin is new Aunt with null record;
   overriding procedure Print(Self : Cousin);

end Classes;