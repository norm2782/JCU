class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: String
  # childTerms :: BackBone.Collection

  initialize: =>
    @set { term: ""
         , childTerms: new Backbone.Collection() }

  term: =>
    @get('term')

  setTerm: (tm) =>
    @set({term: tm})

  childTerms: =>
    @get('childTerms')

  setChildNo: (childNo) =>
    newChildren = new Array()
    newChildren.push(new ProofTreeNode()) for i in [1..childNo] if childNo > 0
    @childTerms().refresh(newChildren)

  isValid: =>
    str = @term()
    if !str?
      return false

    # Token -> a word with possibly spaces in front and after
    # Term  -> Token ( Token {, Token}* ).
    token = "\\s*\\w+\\s*"
    regex = new RegExp(token + "\\(" + token + "(," + token + ")*\\)\\.\\s*$")
    valid = regex.test str
    valid && @childTerms().reduce(((acc, nd) -> nd.isValid() && acc), true)
