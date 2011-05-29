window.app = {}
app.controllers = {}
app.models = {}
app.collections = {}
app.views = {}
app.styles = {}
app.templates = {}

RulesList = require('collections/rules_list_collection').RulesList
ProofTree = require('models/proof_tree_model').ProofTree
MainController = require('controllers/main_controller').MainController
HomeView = require('views/home_view').HomeView
RulesListView = require('views/rules_list_view').RulesListView
ProofTreeView = require('views/proof_tree_view').ProofTreeView

# app bootstrapping on document ready
$(document).ready ->
  app.initialize = ->
    app.collections.rulesList = new RulesList()

    # _.and :: [Bool] -> Bool
    _.mixin {and: (xs) -> _.all xs, _.identity}

    # _.or  :: [Bool] -> Bool
    _.mixin {or:  (xs) -> _.any xs, _.identity}

    # Clone objects
    _.mixin {clone: (obj) -> $.extend(true, {}, obj) }

    app.controllers.main = new MainController()

    app.models.tree = new ProofTree()

    app.views.home = new HomeView()
    app.views.rulesList = new RulesListView()
    app.views.proofTree = new ProofTreeView({model: app.models.tree})

    Backbone.history.saveLocation("home") if Backbone.history.getFragment() is ''
  app.initialize()
  Backbone.history.start()
