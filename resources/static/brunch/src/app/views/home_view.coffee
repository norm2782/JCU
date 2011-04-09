class HomeView extends Backbone.View
  id: 'home-view'

  render: ->
    $(@.el).html app.templates.home()
    $(@.el).find('home-view').append app.views.ruleTree.render().el
    $(@.el).find('home-view').append app.views.ruleList.render().el
    @
