<apply template="layout">
  <bind tag="header"></bind>
  <h1>Please login</h1>
  <form method="post" action="/login">
  <table>
    <tr>
      <td>Email</td><td><input type="text" name="email" /></td>
    </tr>
    <tr>
      <td>Password</td><td><input type="password" name="password" /></td>
    </tr>
    <tr>
      <td><input type="submit" text="Login" /></td>
      <td><input type="checkbox" name="remember" />Remember me?</td>
    </tr>
  </table>
  </form>
</apply>
