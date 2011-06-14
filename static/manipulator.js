function toggle(x)
{
  if(x.css('display') == 'none')
  {
    x.css('display', '');
    x.siblings().css('display', 'none');
  }
  else
  {
    x.css('display', 'none');
    x.siblings().css('display', '');
  }

}

function setConstructor(e, p, i)
{
  var url     = document.URL
  var data    = { action: 'setConstructor'
                , path:   p
                , index:  i
                }
  var success = function(x){ $(e).html(x); $("select").sb(); }

  $.post(url,data, success);
}

 $(document).ready(function(){
   $("select").sb();
 });

