homeTemplate = require('templates/home')

class exports.HomeView extends Backbone.View
  id: 'home-view'

  render: ->
    @$(@.el).html homeTemplate
    @$(@.el).find('#rules-tree-div').append app.views.rulesTree.render().el
    @$(@.el).find('#rules-list-div').append app.views.rulesList.render().el
    @
