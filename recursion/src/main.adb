-- Copyright (C) 2024
-- Jeremiah Breeden
--
-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Main is

    -- Only needed for Ada_95
    function Ada_95_Get_Line return String is
        Buffer_Length : constant := 2;
        Result : String(1..Buffer_Length);
        Last   : Natural := 0;
    begin
        Get_Line(Result, Last);
        if Last = Buffer_Length then
            return Result & Ada_95_Get_Line;
        else
            return Result(1..Last);
        end if;
    end Ada_95_Get_Line;

    -- Types for the overall design
    type Node_Kind is (Question, Answer);
    type Question_Result is (No, Yes);
    
    -- Forward declaration and node access type
    type Node;
    type Node_Access is not null access Node;
    
    -- Holds links to various question results
    type Node_Links is array(Question_Result) of Node_Access;
    
    -- Main node type
    type Node(Kind : Node_Kind := Question) is record
        case Kind is
            when Question => 
                Question : Unbounded_String;
                Links    : Node_Links;
            when Answer =>
                Answer   : Unbounded_String;
        end case;
    end record;
    
    -- Utility conversion operations
    function "+"(Value : String) return Unbounded_String
        renames To_Unbounded_String;
    function "+"(Value : Unbounded_String) return String
        renames To_String;
    
    -- Work horse function
    function Recursive_Guess(Node : Node_Access) return String is
    begin
        -- Terminating condition for recursion
        if Node.Kind = Answer then
            return "My guess is " & (+Node.Answer);
        end if;
        
        -- ask question
        Put(+Node.Question & " ");
        
        -- loop until a valid answer (yes/no) is given
        loop declare
            -- Take the user input here at initialization
            Answer_Str : constant String := Get_Line;
            Answer     : Question_Result;
        begin
            -- append newline to standard out after
            -- the question is answered
            New_Line; 
            
            -- Convert answer to enumeration. Invalid answers
            -- are handled below in an exception handler
            Answer := Question_Result'Value(Answer_Str);
            
            -- Valid answer at this point so call this
            -- function again recursively
            return Recursive_Guess(Node.Links(Answer));
        
        exception
            when Constraint_Error => 
                -- Wrong answer so put error message and loop
                -- again for correct answer
                Put("Invaild Answer, put yes or no: ");
                
        end; end loop; -- end for declare block and loop
        
    end Recursive_Guess;
    
    -- Utility node construction functions
    function New_Answer
        (Answer_Str : String) 
         return Node_Access 
    is begin
        return new Node'(Kind   => Answer, 
                         Answer => +Answer_Str);
    end New_Answer;
        
    function New_Question
        (Question_Str : String;
         No_Node      : Node_Access;
         Yes_Node     : Node_Access)
         return Node_Access
    is begin
        return new Node'(Kind     => Question, 
                         Question => +Question_Str, 
                         Links    =>
                            (No  => No_Node, 
                             Yes => Yes_Node));
    end New_Question;
                     
    -- Test tree
    Root : constant Node_Access :=
        New_Question("Is it a mammal?",
            No_Node => New_Question("Is it a fish?",
                No_Node  => New_Answer("Beetle"),
                Yes_Node => New_Answer("Trout")),
            Yes_Node => New_Answer("Cow"));
    
begin
    Put_Line(Recursive_Guess(Root));
end Main;