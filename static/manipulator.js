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
