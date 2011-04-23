class exports.Rule extends Backbone.Model
  validate: (str) ->
    token    = "\\s*\\w+\\s*"
    opttoken = "(," + token + ")?"
    rule     = "\\w+\\(" + token + opttoken + "\\)"
    regex    = new RegExp("(\\.|:-(" + rule + "(,\\s*|\\.))*)")
    regex.test str

  clear: ->
    @destroy()
    @view.remove()
