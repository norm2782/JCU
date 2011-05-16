rulesListItemTemplate = require('templates/rules_list_item')

class exports.RulesListItemView extends Backbone.View
  tagName: 'li'

  events:
    "click .btnDeleteList" : "deleteItem"

  initialize: ->
    _.bindAll(@, 'addOne', 'addAll', 'render')
    app.collections.rulesTree.bind 'add', @addOne
    app.collections.rulesTree.bind 'refresh', @addAll
    app.collections.rulesTree.bind 'all', @renderList
    app.collections.rulesTree.fetch()

  deleteItem: ->
    @model.destroy()
    @$(@el).remove()

  render: =>
    @$(@el).html rulesListItemTemplate content: @model.toJSON()
    @

