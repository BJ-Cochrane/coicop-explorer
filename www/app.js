// Code links anywhere on the page open that category
$(document).on('click', '.code-link', function (e) {
  e.preventDefault();
  Shiny.setInputValue('goto', { v: $(this).attr('data-v'), code: String($(this).attr('data-code')) },
                      { priority: 'event' });
});

// Example searches
$(document).on('click', '.try-link', function (e) {
  e.preventDefault();
  $('#q').val($(this).attr('data-q')).trigger('input').trigger('change').focus();
});

// "/" jumps to the search box, Esc clears it
$(document).on('keydown', function (e) {
  var typing = /input|textarea|select/i.test(e.target.tagName);
  if (e.key === '/' && !typing) { e.preventDefault(); $('#q').focus().select(); }
  if (e.key === 'Escape' && e.target.id === 'q') { $('#q').val('').trigger('input'); }
});

// On small screens the detail sits above the results, so scroll up to it
$(document).on('shiny:inputchanged', function (e) {
  if ((e.name === 'pick' || e.name === 'goto') && window.innerWidth < 992) {
    setTimeout(function () {
      var el = document.querySelector('.detail');
      if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
    }, 400);
  }
});
