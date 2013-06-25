XMLHttpRequest est un objet JavaScript qui a été créé par Microsoft et adopté par Mozilla. Vous pouvez l'utiliser pour récupérer facilement des données via HTTP. En dépit de son nom, il a d'autres usages que de seuls documents XML. Dans Gecko, cet objet implémente les interfaces nsIJSXMLHttpRequest et nsIXMLHttpRequest. Les versions récentes de Gecko ont fait subir quelques modifications à cet objet (voir Changements dans XMLHttpRequest pour Gecko 1.8).

Utilisation basique

L'utilisation de XMLHttpRequest est très simple. Il vous suffit de créer une instance de cet objet, d'ouvrir une URL et d'envoyer la requête. Le code d'état HTTP de la réponse, ainsi que le document résultant sont disponibles dans cet objet après la requête.

Note: Les versions de Firefox antérieures à la version 3 envoient toujours la requête à l'aide de l'encodage UTF-8 ; Firefox 3 envoie correctement le document à l'aide de l'encodage spécifié par data.xmlEncoding, ou en UTF-8 si aucun encodage n'est spécifié.
Exemple

var req = new XMLHttpRequest();
req.open('GET', 'http://www.mozilla.org/', false); 
req.send(null);
if(req.status == 200)
  dump(req.responseText);
Note : Cet exemple fonctionne de façon synchrone, donc il va bloquer l'interface utilisateur lorsque vous l'appellerez depuis JavaScript. Il est déconseillé en pratique de procéder ainsi.
