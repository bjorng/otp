-module('6').

-export([get_file_header/1]).

get_file_header(BCD) ->
    ToGet = ex:cd_file_header_from_bin(),
    {B2, BCDRest} =
        case BCD of
            <<_:4/binary,G:ToGet/binary,Rest/binary>> ->
                {G, Rest}
        end,
    e:get_filename_from_b2(e:f(), e:f()).

