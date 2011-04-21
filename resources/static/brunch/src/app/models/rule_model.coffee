class exports.Rule extends Backbone.Model
  rule: ""

  clear: ->
    @destroy()
    @view.remove()
