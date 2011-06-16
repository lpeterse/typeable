function setExpansion(x,p,b)
{
  x.parent().children('.contentTable').css('display', b?'':'none');
  x.parent().children('.typeTable').css('display',    b?'none':'');
  $.post(document.URL, { action: 'setExpansion', path: p, expanded: b?'True':'False' });
}

function setConstructor(e, p, i)
{
  var url     = document.URL
  var data    = { action: 'setConstructor'
                , path:   p
                , index:  i
                }
  var success = function(x){ $(e).html(x); $("select").sb(); hintUndefines(); }

  $.post(url,data, success);
}


function hintUndefines(e)
{
  if(!e)
  {
    $('#treeContainer').children('table').each(function(i,x){ hintUndefines(x); });
  }
  if($(e).hasClass('contentUndefined'))
  {
    return true;
  }
  else
  {
    var myObject         = new Object();
    myObject.isUndefined = false;
    myObject.update      = function(bl) { this.isUndefined = this.isUndefined || bl; };
    var ch = $(e).children('tbody').children('tr').children('td').children('table').each( function(i,x) { myObject.update(hintUndefines(x)); } );

    if(myObject.isUndefined)
    {
      $(e).children('tbody').children('tr').children('td').addClass('undefinedHint');
      return true;
    }
    else
    {
      $(e).children('tbody').children('tr').children('td').removeClass('undefinedHint');
      return false;
    }
  }
}

 $(document).ready(function(){
   $("select").sb();
   hintUndefines();
 });

