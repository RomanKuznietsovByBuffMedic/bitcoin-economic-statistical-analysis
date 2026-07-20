function(el, x) {
  const body = document.body;

  function cssVariable(styles, name, fallback) {
    const value = styles.getPropertyValue(name).trim();
    return value || fallback;
  }

  function colourChannels(value) {
    const colour = value.trim().toLowerCase();
    const shortHex = colour.match(/^#([0-9a-f]{3})$/i);
    const longHex = colour.match(/^#([0-9a-f]{6})$/i);

    if (shortHex) {
      return shortHex[1].split('').map(function(channel) {
        return Number.parseInt(channel + channel, 16);
      });
    }

    if (longHex) {
      return [0, 2, 4].map(function(index) {
        return Number.parseInt(longHex[1].slice(index, index + 2), 16);
      });
    }

    const rgb = colour.match(
      /^rgba?\(\s*([\d.]+)[,\s]+([\d.]+)[,\s]+([\d.]+)/i
    );

    if (rgb) {
      return rgb.slice(1, 4).map(Number);
    }

    return null;
  }

  function hasDarkBackground(value) {
    const channels = colourChannels(value);

    if (!channels) {
      return false;
    }

    const brightness = (
      299 * channels[0] +
      587 * channels[1] +
      114 * channels[2]
    ) / 1000;

    return brightness < 140;
  }

  function applyTheme() {
    const styles = window.getComputedStyle(body);
    const hasDarkClass =
      body.classList.contains('quarto-dark') ||
      document.documentElement.classList.contains('quarto-dark');

    const background = cssVariable(
      styles,
      '--bs-body-bg',
      hasDarkClass ? '#181a1b' : '#ffffff'
    );
    const isDark = hasDarkClass || hasDarkBackground(background);
    const text = cssVariable(
      styles,
      '--bs-body-color',
      isDark ? '#e8e6e3' : '#212529'
    );
    const fontFamily = styles.fontFamily || 'system-ui, sans-serif';
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
    const configuredHeight = Number.parseFloat(
      cssVariable(
        styles,
        '--book-interactive-chart-height',
        '720px'
      )
    );

    const layout = {
      paper_bgcolor: background,
      plot_bgcolor: background,
      'font.family': fontFamily,
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
      'hoverlabel.font.family': fontFamily,
      'hoverlabel.font.color': text,
      'modebar.bgcolor': 'rgba(0, 0, 0, 0)',
      'modebar.color': text,
      'modebar.activecolor': link
    };

    if (Number.isFinite(configuredHeight) && configuredHeight > 0) {
      el.style.setProperty(
        'height',
        configuredHeight + 'px',
        'important'
      );
      el.style.setProperty(
        'min-height',
        configuredHeight + 'px',
        'important'
      );
      layout.height = configuredHeight;
    }

    const annotations = el.layout.annotations || [];
    annotations.forEach(function(annotation, index) {
      layout['annotations[' + index + '].font.color'] = text;
    });

    Plotly.relayout(el, layout);

    window.requestAnimationFrame(function() {
      Plotly.Plots.resize(el);
    });

    (el.data || []).forEach(function(trace, index) {
      const mode = trace.mode || '';

      if (mode.includes('text') || trace.type === 'bar') {
        Plotly.restyle(
          el,
          {
            'textfont.color': text
          },
          [index]
        );
      }

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

  el.quartoThemeObserver.observe(document.documentElement, {
    attributes: true,
    attributeFilter: ['class']
  });
}
