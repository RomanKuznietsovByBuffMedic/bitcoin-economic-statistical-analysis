function(el, x) {
  const body = document.body;

  function cssVariable(styles, name, fallback) {
    const value = styles.getPropertyValue(name).trim();
    return value || fallback;
  }

  function applyTheme() {
    const styles = window.getComputedStyle(body);
    const isDark = body.classList.contains('quarto-dark');

    const background = cssVariable(
      styles,
      '--bs-body-bg',
      isDark ? '#181a1b' : '#ffffff'
    );
    const text = cssVariable(
      styles,
      '--bs-body-color',
      isDark ? '#e8e6e3' : '#212529'
    );
    const link = cssVariable(
      styles,
      '--bs-link-color',
      isDark ? '#75aadb' : '#0d6efd'
    );
    const grid = isDark
      ? 'rgba(232, 230, 227, 0.16)'
      : 'rgba(33, 37, 41, 0.14)';
    const axis = isDark
      ? 'rgba(232, 230, 227, 0.42)'
      : 'rgba(33, 37, 41, 0.35)';
    const hover = isDark ? '#24282c' : '#ffffff';

    const layout = {
      paper_bgcolor: background,
      plot_bgcolor: background,
      'font.color': text,
      'title.font.color': text,
      'legend.font.color': text,
      'legend.bgcolor': 'rgba(0, 0, 0, 0)',
      'xaxis.color': text,
      'xaxis.tickfont.color': text,
      'xaxis.title.font.color': text,
      'xaxis.gridcolor': grid,
      'xaxis.linecolor': axis,
      'xaxis.zerolinecolor': axis,
      'yaxis.color': text,
      'yaxis.tickfont.color': text,
      'yaxis.title.font.color': text,
      'yaxis.gridcolor': grid,
      'yaxis.linecolor': axis,
      'yaxis.zerolinecolor': axis,
      'hoverlabel.bgcolor': hover,
      'hoverlabel.bordercolor': axis,
      'hoverlabel.font.color': text,
      'modebar.bgcolor': 'rgba(0, 0, 0, 0)',
      'modebar.color': text,
      'modebar.activecolor': link
    };

    const annotations = el.layout.annotations || [];
    annotations.forEach(function(annotation, index) {
      layout['annotations[' + index + '].font.color'] = text;
    });

    Plotly.relayout(el, layout);

    (el.data || []).forEach(function(trace, index) {
      if (trace.name === 'Фактична') {
        Plotly.restyle(
          el,
          {
            'line.color': isDark ? '#b8c2cc' : '#52606d'
          },
          [index]
        );
      }
    });
  }

  applyTheme();

  if (el.quartoThemeObserver) {
    el.quartoThemeObserver.disconnect();
  }

  el.quartoThemeObserver = new MutationObserver(function() {
    window.requestAnimationFrame(applyTheme);
  });

  el.quartoThemeObserver.observe(body, {
    attributes: true,
    attributeFilter: ['class']
  });
}
