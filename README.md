Record unified manipulation
===========================

It is a tiny parse transform.

__License__: MIT

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))

[![Build Status](https://secure.travis-ci.org/freeakk/rum.png?branch=master)](http://travis-ci.org/freeakk/rum)


This library provides several functions and guards for manipulations 
with records.


Guards
-----

* `default(RecordIndex) -> term().`

    ```erlang
    -record(pet, {head = cat, tail = octo}).

    default(#pet.head) is cat.
    Dog = #pet{head = dog, tail = dog}.
    ```
* `is_default(RecordAccess) -> boolean().` 

    ```erlang
    is_default(Dog#pet.head) is false.
    ```


Other functions
---------------

* `old()` and `default()` are used for simple updates:

    ```erlang
    Dog#pet{head = atom_to_list(old()), tail = atom_to_list(old())}
    is #pet{head = "dog", tail = "dog"}.
    ```

    ```erlang
    Dog#pet{head = default()}
    is #pet{head = cat, tail = dog}.
    ```

Examples
--------

Before:

```erlang
put(Store, Ref, Hash)
    when is_reference(Ref), is_integer(Hash) ->
    #qlc_table_hash_register{hash_to_ref = H2R, ref_to_hash = R2H} = Store,
    NewR2H = gb_trees:insert(Ref, Hash, R2H),
    NewH2R = gb_trees:insert(Hash, Ref, H2R),
    Store#qlc_table_hash_register{hash_to_ref = NewH2R, ref_to_hash = NewR2H}.


erase(Store, Key) ->
    #qlc_table_hash_register{hash_to_ref = H2R, ref_to_hash = R2H} = Store,
    case key_to_ref_and_hash(Store, Key) of
    {ok, {Ref, Hash}} ->
        NewR2H = gb_trees:delete(Ref, R2H),
        NewH2R = gb_trees:delete(Hash, H2R),
        NewStore = Store#qlc_table_hash_register{hash_to_ref = NewH2R,
                                                 ref_to_hash = NewR2H},
        {ok, {NewStore, Ref, Hash}};

    {error, _Reason} = Error ->
        Error
    end.
```

After:

```erlang
put(Store, Ref, Hash)
    when is_reference(Ref), is_integer(Hash) ->
    Store#qlc_table_hash_register{
        hash_to_ref = gb_trees:insert(Ref, Hash, old()),
        ref_to_hash = gb_trees:insert(Hash, Ref, old())}.


erase(Store, Key) ->
    case key_to_ref_and_hash(Store, Key) of
    {ok, {Ref, Hash}} ->
        NewStore = Store#qlc_table_hash_register{
            hash_to_ref = gb_trees:delete(Hash, old()),
            ref_to_hash = gb_trees:delete(Ref, old()},
        {ok, {NewStore, Ref, Hash}};

    {error, _Reason} = Error ->
        Error
    end.
```

