Record unified manipulation
===========================

It is a tiny parse transform.

**License**: MIT

**Author**: Uvarov Michael (freeakk@gmail.com)


.. image:: https://secure.travis-ci.org/mad-cocktail/rum.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/mad-cocktail/rum


This library provides several functions and guards for manipulations
with records.

Guards
------

-  ``default(RecordIndex) -> term().``

.. code-block:: erlang

       -record(pet, {head = cat, tail = octo}).
       > default(#pet.head). 
       cat. 

-  ``is_default(RecordAccess) -> boolean().``

.. code-block:: erlang

       > Dog = #pet{head = dog, tail = dog}.
       > is_default(Dog#pet.head).
       false.

Other functions
---------------

-  ``old()`` and ``default()`` functions are used for simple updates:

.. code-block:: erlang

       > Dog#pet{head = atom_to_list(old()), tail = atom_to_list(old())}.
       #pet{head = "dog", tail = "dog"}.

.. code-block:: erlang

       > Dog#pet{head = default()}.
       #pet{head = cat, tail = dog}.

-  ``with()`` is a variant of ``old()`` for nested updates:

.. code-block:: erlang

       > Dog#pet{head = with(Head, atom_to_list(Head)), 
       >         tail = with(Tail, atom_to_list(Tail))}.
       #pet{head = "dog", tail = "dog"}.

Example 1
---------

Before:

.. code-block:: erlang

    put(Store, Ref, Hash) ->
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

After:

.. code-block:: erlang

    put(Store, Ref, Hash) ->
        Store#qlc_table_hash_register{
            hash_to_ref = gb_trees:insert(Ref, Hash, old()),
            ref_to_hash = gb_trees:insert(Hash, Ref, old())}.


    erase(Store, Key) ->
        case key_to_ref_and_hash(Store, Key) of
        {ok, {Ref, Hash}} ->
            NewStore = Store#qlc_table_hash_register{
                hash_to_ref = gb_trees:delete(Hash, old()),
                ref_to_hash = gb_trees:delete(Ref, old())},
            {ok, {NewStore, Ref, Hash}};

        {error, _Reason} = Error ->
            Error
        end.

Example 2
---------

``B`` is bound to ``A#a.a`` and C is bound to ``B#b.b`` (``A#a.a#b.b``)
in this example:

.. code-block:: erlang

    -record(a, {a}).
    -record(b, {b}).
    -record(c, {c}).

    abc_set(A, X) ->                             
        A#a{a = with(B,                          
                     B#b{b = with(C,             
                                  C#c{c = X})})}.
                                  
This will be expanded to:

.. code-block:: erlang

    -record(a, {a}).
    -record(b, {b}).
    -record(c, {c}).

    abc_set(A, X) ->                             
        A#a{a = A#a.a                         
         #b{b = A#a.a#b.b
         #c{c = X}}}.
