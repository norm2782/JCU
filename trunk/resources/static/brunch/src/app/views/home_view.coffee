class HomeView extends Backbone.View
  id: 'home-view'

  render: ->
    $(@.el).html app.templates.home()
    $(@.el).find('#rules-tree-div').append app.views.ruleTree.render().el
    $(@.el).find('#rules-list-div').append app.views.ruleList.render().el
    @
