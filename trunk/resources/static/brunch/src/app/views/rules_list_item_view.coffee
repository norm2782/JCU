class RulesListItemView extends Backbone.View
  tagName: 'li'

  initialize: ->
    @model.view = @

  render: =>
    $(@el).html app.templates.rulesListItem(content: @model.toJSON())
    @

  remove: ->
    $(@el).remove()

  clear: ->
    @model.clear()

