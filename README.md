Making Chunkify in `couch_btree` faster
==

Exploring CouchDB internals I decided to begin with `couch_btree` as it's something familiar from other contexts. I looked first at `chunkify` since the comments indicate it's a place where improvement is needed.

    %%%%%%%%%%%%% The chunkify function sucks! %%%%%%%%%%%%% 
    % It is inaccurate as it does not account for compression when blocks are
    % written. Plus with the "case size(term_to_binary(InList)) of" code it's
    % probably really inefficient.
    chunkify(_Bt, []) ->
        [];
    chunkify(Bt, InList) ->
        case size(term_to_binary(InList)) of
        Size when Size > ?CHUNK_THRESHOLD ->
            NumberOfChunksLikely = ((Size div ?CHUNK_THRESHOLD) + 1),
            ChunkThreshold = Size div NumberOfChunksLikely,
            chunkify(Bt, InList, ChunkThreshold, [], 0, []);
        _Else ->
            [InList]
        end.

    chunkify(_Bt, [], _ChunkThreshold, [], 0, OutputChunks) ->
        lists:reverse(OutputChunks);
    chunkify(_Bt, [], _ChunkThreshold, OutList, _OutListSize, OutputChunks) ->      
        lists:reverse([lists:reverse(OutList) | OutputChunks]);
    chunkify(Bt, [InElement | RestInList], ChunkThreshold, OutList, OutListSize, OutputChunks) ->
        case size(term_to_binary(InElement)) of
        Size when (Size + OutListSize) > ChunkThreshold andalso OutList /= [] ->
            chunkify(Bt, RestInList, ChunkThreshold, [], 0, [lists:reverse([InElement | OutList]) | OutputChunks]);
        Size ->
            chunkify(Bt, RestInList, ChunkThreshold, [InElement | OutList], OutListSize + Size, OutputChunks)
        end.

First a nit: `chunkify` does not need the b-tree passed in as an argument. It never uses it.  

It would be nice to not need the first call to `term_to_binary`. My first pass at this did exactly that. It went ahead and chunked regardless. This resulted in a larger file running the tests. So is this first call to `term_to_binary` intended to compute a number of chunks of roughly the same size?

So keeping the exct same semantics my next attempt reworked `chunkify` to use `foldl`, thinking that using a builtin might be more optimal that an explicit recursion. This only results in at best a 4% improvement in the case of N=1000 

    %%
    %% A lame attempt at improving chunkify, seems to pick up 4% or so for large test cases
    %% 
    %%
    chunkify1([]) ->
        [];

    chunkify1(InList) ->
        InSize = size(term_to_binary(InList)),
        NeedToChunk = InSize > ?CHUNK_THRESHOLD,
        if NeedToChunk ->
                NumberOfChunksLikely = ((InSize div ?CHUNK_THRESHOLD) + 1),
                ChunkThreshold = InSize div NumberOfChunksLikely,
                io:format("Number of chunks is: ~w, each of size ~w ~n",[NumberOfChunksLikely, ChunkThreshold]),
                [CurrentChunk, _, AllChunks] =  lists:foldl(fun(Elem, [OutList, OutListSize, Output]) ->
                            case size(term_to_binary(Elem)) of
                                Size when (Size + OutListSize) > ChunkThreshold andalso OutList /= [] ->
                                    [[], 0, [lists:reverse([Elem | OutList]) | Output]]; 
                                Size ->
                                    [[Elem | OutList], Size + OutListSize, Output]
                            end
                     end,[[],0,[]],InList),
                if length(CurrentChunk) > 0 ->
                        lists:reverse([lists:reverse(CurrentChunk) | AllChunks]);
                   true ->
                        lists:reverse(AllChunks)
                end;
           true ->
                [InList]
        end.
