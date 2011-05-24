RulesListItemView = require('views/rules_list_item_view').RulesListItemView

class exports.RulesListView extends Backbone.View

  id: 'rules-list-view'
  tagName: 'ul'

  initialize: =>
    app.collections.rulesList.bind 'add', @addOne
    app.collections.rulesList.bind 'refresh', @addAll
    app.collections.rulesList.bind 'all', @renderList
    app.collections.rulesList.fetch()

  addOne: (rule) =>
    view = new RulesListItemView model: rule
    @$(@el).append view.render().el

  addAll: =>
    app.collections.rulesList.each @addOne

  renderList: =>
    app.views.rulesList.render()
    @$('.draggable').draggable({ revert: true
                               , revertDuration: 100
                               , start: -> $('#proof-tree-view input[type=text]').blur() })
