-module('5').

-export([bin_gen_field/7]).

bin_gen_field({bin_element, Anno, VE, Size0, Options0},
              Bin, Bs0, BBs0, Mfun, Efun, ErrorFun) ->
    {Size1, [Type, {unit, Unit}, Sign, Endian]} =
        ex:make_bit_type(Anno, Size0, Options0, ErrorFun),
    V = erl_eval:partial_eval(VE),
    NewV = ex:coerce_to_float(V, Type),
    case catch Efun(Size1, BBs0) of
        {value, Size, _BBs} ->
            bin_gen_field1(Bin, Type, Size, Unit, Sign, Endian, NewV,
                           Bs0, BBs0, Mfun);
        _ ->
            done
    end.

bin_gen_field1(Bin, float, Size, Unit, Sign, Endian, NewV, Bs0, BBs0,
               Mfun) ->
    case catch ex:get_value(Bin, float, Size, Unit, Sign, Endian) of
        {Val, <<_/bitstring>> = Rest} ->
            case catch Mfun(match, {NewV, Val, Bs0}) of
                {match, Bs} ->
                    BBs = ex:add_bin_binding(Mfun, NewV, Bs, BBs0),
                    {match, Bs, BBs, Rest};
                _ ->
                    {nomatch, Rest}
            end;
        _ ->
            case
                catch
                    ex:get_value(Bin, integer, Size, Unit, Sign, Endian)
            of
                {_, <<_/bitstring>> = Rest} ->
                    {nomatch, Rest};
                _ ->
                    done
            end
    end.

