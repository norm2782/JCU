rulesListItemTemplate = require('templates/rules_list_item')

class exports.RulesListItemView extends Backbone.View
  tagName: 'li'

  events:
    "click .btnDeleteList" : "deleteItem"

  initialize: ->
    _.bindAll @, 'render'

  deleteItem: ->
    @model.destroy()
    @$(@el).remove()

  render: =>
    @$(@el).html rulesListItemTemplate content: @model.toJSON()
    @

