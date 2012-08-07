-module(rum).
-export([parse_transform/2]).


oneof_function(Fs) ->
    fun(Node) ->
        Apply = fun(F, N) -> F(N) end,
        lists:foldl(Apply, Node, Fs)
    end.


parse_transform(Forms, _Options) ->
    %% Extract record 
    Records = extract_record_definitions(Forms),
    %% RecordAndFieldNames2DefaultFieldValueOrdDict
    RFN2DFV = orddict:from_list(record_definitions_to_pl(Records)),
    F1 = replace_record_expr_value(default_rec_expr_trans(RFN2DFV)),
    F2 = replace_record_expr_value(fun old_rec_expr_trans/3),
    F3 = is_default_trans(RFN2DFV),
    F4 = default_trans(RFN2DFV),
    F  = oneof_function([F1, F2, F3, F4]),
    X = [postorder(F, Tree) || Tree <- Forms],
    io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


extract_record_definitions(Forms) ->
    [erl_syntax:attribute_arguments(F) 
       || F <- Forms, erl_syntax:type(F) =:= attribute, 
          erl_syntax:atom_value(erl_syntax:attribute_name(F)) =:= record].



-spec record_definitions_to_pl(Recs) -> PL when
    Recs :: [erl_syntax:syntaxTree()],
    PL   :: [{Key, erl_syntax:syntaxTree()}],
    Key  :: {RecName, FieldName},
    RecName :: atom(),
    FieldName :: atom().

record_definitions_to_pl(Recs) ->
    [to_element(Field, RecName)
        || [RecName, RecFields] <- Recs, Field <- erl_syntax:tuple_elements(RecFields)].


-spec to_element(Field, RecName) -> Elem when
    Field :: erl_syntax:syntaxTree(),
    RecName :: erl_syntax:syntaxTree(),
    Elem :: {Key, FieldValue},
    FieldValue :: erl_syntax:syntaxTree(),
    Key :: {RecNameAtom, FieldNameAtom},
    RecNameAtom :: atom(),
    FieldNameAtom :: atom().
    
to_element(Field, RecName) ->
    FieldName     = erl_syntax:record_field_name(Field),
    FieldValue    = erl_syntax:record_field_value(Field),
    {to_key(FieldName, RecName), FieldValue}.


-spec to_key(FieldName, RecName) -> Key when
    FieldName :: erl_syntax:syntaxTree(),
    RecName :: erl_syntax:syntaxTree(),
    Key :: {RecNameAtom, FieldNameAtom},
    RecNameAtom :: atom(),
    FieldNameAtom :: atom().

to_key(FieldName, RecName) ->
    FieldNameAtom = erl_syntax:atom_value(FieldName),
    RecNameAtom   = erl_syntax:atom_value(RecName),
    {RecNameAtom, FieldNameAtom}.


postorder(F, Form) ->
    NewTree =
        case erl_syntax:subtrees(Form) of
        [] ->
            Form;
        List ->
            Groups = [handle_group(F, Group) || Group <- List],
            Tree2 = erl_syntax:update_tree(Form, Groups),
            Form2 = erl_syntax:revert(Tree2),
            Form2
        end,
    F(NewTree).


replace_record_expr_value(TransFun) ->
    fun(Node) ->
        case erl_syntax:type(Node) of
        record_expr ->
            Argument  = erl_syntax:record_expr_argument(Node),
            Type      = erl_syntax:record_expr_type(Node),
            Fields    = erl_syntax:record_expr_fields(Node),
            NewFields = 
            [ TransFun(Argument, Type, Field) || Field <- Fields ],
            erl_syntax:record_expr(Argument, Type, NewFields);
        _NodeType ->
            Node
        end
        end.


%% `Argument#Type{Field}', where Field is `Name = Value'.
%% see example 3.
default_rec_expr_trans(RFN2DFV) ->
    fun(_Argument, Type, Field) ->
        Value = erl_syntax:record_field_value(Field),
        Name  = erl_syntax:record_field_name(Field),
        F = fun(Node) ->
            IsFun = is_local_function(Node, default, 0),
            if IsFun -> record_default_value(RFN2DFV, Type, Name);
               true  -> Node
                end
            end,
        NewValue = postorder(F, Value),
        update_field(Field, NewValue)
        end.


%% see example 4.
%% `Argument#Type{Field}', where Field is `Name = Value'.
old_rec_expr_trans(none, _Type, Field) ->
    Field; %% skip it
old_rec_expr_trans(Argument, Type, Field) ->
    case erl_syntax:type(Argument) of
    variable ->
        Value = erl_syntax:record_field_value(Field),
        Name  = erl_syntax:record_field_name(Field),
        ArgName = erl_syntax:variable_name(Argument),
        F = fun(Node) ->
            IsFun0 = is_local_function(Node, old, 0),
            IsFun1 = is_local_function(Node, old, 1),
            if 
            %% Search for all matches.
            IsFun0 -> 
                erl_syntax:record_access(Argument, Type, Name);
            %% Search by the variable name.
            IsFun1 ->
                [WantedArg] = erl_syntax:application_arguments(Node),
                IsWanted = erl_syntax:type(WantedArg) =:= variable
                   andalso erl_syntax:variable_name(WantedArg) =:= ArgName,
                if IsWanted -> erl_syntax:record_access(Argument, Type, Name);
                   true     -> Node 
                end;
            true   -> Node
            end
        end,
        NewValue = postorder(F, Value),
        update_field(Field, NewValue)
    end.


%% see example 1.
-spec record_default_value(RFN2DFV, Type, Name) -> Value when
    RFN2DFV :: orddict:ordict(),
    Type :: erl_syntax:syntaxTree(),
    Name :: erl_syntax:syntaxTree(),
    Value :: erl_syntax:syntaxTree().

record_default_value(RFN2DFV, Type, Name) ->
    Value = orddict:fetch(to_key(Name, Type), RFN2DFV),
    [erlang:error(unknown_record_field_name) || Value =:= undefined],
    Value.


%% Return `NewField', where its value is replaced by `NewValue'.
-spec update_field(Field, NewValue) -> NewField when
    Field :: erl_syntax:syntaxTree(),
    NewField :: erl_syntax:syntaxTree(),
    NewValue :: erl_syntax:syntaxTree().

update_field(Field, NewValue) ->
    Name = erl_syntax:record_field_name(Field),
    erl_syntax:record_field(Name, NewValue).


-spec is_local_function(Node, FunName, FunArity) -> boolean() when 
    Node :: erl_syntax:syntaxTree(),
    FunName :: atom(),
    FunArity :: non_neg_integer().

is_local_function(Node, FunName, FunArity) ->
    erl_syntax:type(Node) =:= application
        andalso always(Op = erl_syntax:application_operator(Node))
        andalso erl_syntax:type(Op) =:= atom
        andalso erl_syntax:atom_value(Op) =:= FunName
        andalso application_arity(Node) =:= FunArity.
        

always(_) -> true.


is_default_trans(RFN2DFV) ->
    fun(Node) ->
        IsFun = is_local_function(Node, is_default, 1),
        if IsFun -> do_is_default_trans(RFN2DFV, Node);
            true -> Node
             end
        end.


%% `is_default(Argument#Type.Field)'
do_is_default_trans(RFN2DFV, Node) ->
    [RecAccess] = erl_syntax:application_arguments(Node),
    case erl_syntax:type(RecAccess) of
    record_access ->
            Name     = erl_syntax:record_access_field(RecAccess),
            Type     = erl_syntax:record_access_type(RecAccess),
            DefValue = record_default_value(RFN2DFV, Type, Name),
            EqOp     = erl_syntax:operator('=:='),
            IsEqual = erl_syntax:infix_expr(RecAccess, EqOp, DefValue),
            erl_syntax:parentheses(IsEqual);
    _OtherType ->
        Node
    end.
        

%% `default(#Type.Field)'
default_trans(RFN2DFV) ->
    fun(Node) ->
        IsFun = is_local_function(Node, default, 1),
        if IsFun -> do_default_trans(RFN2DFV, Node);
            true -> Node
             end
        end.


do_default_trans(RFN2DFV, Node) ->
    [RecIndex] = erl_syntax:application_arguments(Node),
    case erl_syntax:type(RecIndex) of 
    record_index_expr ->
        Name     = erl_syntax:record_index_expr_field(RecIndex),
        Type     = erl_syntax:record_index_expr_type(RecIndex),
        record_default_value(RFN2DFV, Type, Name);
    _OtherType ->
        Node
    end.


application_arity(AppNode) ->
    length(erl_syntax:application_arguments(AppNode)).
        
    
handle_group(F, Group) ->
    [postorder(F, Subtree) || Subtree <- Group].
