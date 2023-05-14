val from_string : (Parser.token, 'b) MenhirLib.Convert.traditional -> string -> 'b
val from_channel :  (Parser.token, 'b) MenhirLib.Convert.traditional -> in_channel -> 'b