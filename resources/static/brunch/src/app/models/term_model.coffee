class exports.Term extends Backbone.Model
  # Attributes:
  # term :: String

  validate: (str) =>
    if !str?
      if !@get('term')?
        return false
      str = @get "term"

    # Token -> a word with possibly spaces in front and after
    # Term  -> Token ( Token {, Token}* ).
    token = "\\s*\\w+\\s*"
    regex = new RegExp(token + "\\(" + token + "(," + token + ")*\\)\\.\\s*$")
    regex.test str
