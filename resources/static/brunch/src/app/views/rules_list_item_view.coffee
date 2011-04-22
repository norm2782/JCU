rulesListItemTemplate = require('templates/rules_list_item')

class exports.RulesListItemView extends Backbone.View
  tagName: 'li'

  events:
    "click .btnDeleteList" : "deleteItem"

  deleteItem: ->
    @model.destroy()
    @$(@el).remove()

  initialize: ->
    @model.view = @

  render: =>
    @$(@el).html rulesListItemTemplate content: @model.toJSON()
    @

  remove: ->
    @$(@el).remove()

  clear: ->
    @model.clear()

