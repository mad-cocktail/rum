-module(rum).
-export([parse_transform/2]).

oneof_function(Fs) ->
    fun(Node) ->
        Apply = fun(F, N) -> F(N) end,
        lists:foldl(Apply, Node, Fs)
    end.


parse_transform(Forms, _Options) ->
    F1 = replace_record_expr_value(fun default_rec_expr_trans/3),
    F2 = replace_record_expr_value(fun old_rec_expr_trans/3),
    F3 = fun is_default_trans/1,
    F4 = fun default_trans/1,
    F  = oneof_function([F1, F2, F3, F4]),
    X = [postorder(F, Tree) || Tree <- Forms],
%   io:format(user, "Before:\t~p\n\nAfter:\t~p\n", [Forms, X]),
    X.


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
default_rec_expr_trans(_Argument, Type, Field) ->
    Value = erl_syntax:record_field_value(Field),
    Name  = erl_syntax:record_field_name(Field),
    F = fun(Node) ->
        IsFun = is_local_function(Node, default, 0),
        if IsFun -> record_default_value(Type, Name);
           true  -> Node
            end
        end,
    NewValue = postorder(F, Value),
    update_field(Field, NewValue).


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
-spec record_default_value(Type, Name) -> Value when
    Type :: erl_syntax:syntaxTree(),
    Name :: erl_syntax:syntaxTree(),
    Value :: erl_syntax:syntaxTree().

record_default_value(Type, Name) ->
    Function = erl_syntax:atom(element),
    Con = erl_syntax:record_expr(none, Type, []),
    %% #Type.Name
    Index = erl_syntax:record_index_expr(Type, Name),
    Arguments = [Index, Con],
    erl_syntax:application(none, Function, Arguments).


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


is_default_trans(Node) ->
    IsFun = is_local_function(Node, is_default, 1),
    if IsFun -> do_is_default_trans(Node);
        true -> Node
         end.


%% `is_default(Argument#Type.Field)'
do_is_default_trans(Node) ->
    [RecAccess] = erl_syntax:application_arguments(Node),
    case erl_syntax:type(RecAccess) of
    record_access ->
            Name     = erl_syntax:record_access_field(RecAccess),
            Type     = erl_syntax:record_access_type(RecAccess),
            DefValue = record_default_value(Type, Name),
            EqOp     = erl_syntax:operator('=:='),
            IsEqual = erl_syntax:infix_expr(RecAccess, EqOp, DefValue),
            erl_syntax:parentheses(IsEqual);
    _OtherType ->
        Node
    end.
        

%% `default(#Type.Field)'
default_trans(Node) ->
    IsFun = is_local_function(Node, default, 1),
    if IsFun -> do_default_trans(Node);
        true -> Node
         end.


do_default_trans(Node) ->
    [RecIndex] = erl_syntax:application_arguments(Node),
    case erl_syntax:type(RecIndex) of 
    record_index_expr ->
        Name     = erl_syntax:record_index_expr_field(RecIndex),
        Type     = erl_syntax:record_index_expr_type(RecIndex),
        record_default_value(Type, Name);
    _OtherType ->
        Node
    end.


application_arity(AppNode) ->
    length(erl_syntax:application_arguments(AppNode)).
        
    
handle_group(F, Group) ->
    [postorder(F, Subtree) || Subtree <- Group].
