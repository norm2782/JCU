class RulesListItemView extends Backbone.View
  tagName: 'li'

  events:
    "click .btnDeleteList" : "deleteItem"

  deleteItem: ->
    @model.destroy()
    $(@el).remove()

  initialize: ->
    @model.view = @

  render: =>
    $(@el).html app.templates.rulesListItem content: @model.toJSON()
    @

  remove: ->
    $(@el).remove()

  clear: ->
    @model.clear()

