function(el, x, data) {
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
        return Number.parseInt(
          longHex[1].slice(index, index + 2),
          16
        );
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

  function requestedChartHeight(styles) {
    const suppliedHeight = Number.parseFloat(
      data && data.height
    );

    if (
      Number.isFinite(suppliedHeight) &&
      suppliedHeight > 0
    ) {
      return suppliedHeight;
    }

    return Number.parseFloat(
      cssVariable(
        styles,
        '--book-interactive-chart-height',
        '720px'
      )
    );
  }

  function styleSeriesToggleMenus(theme) {
    const menus = (
      el.layout &&
      Array.isArray(el.layout.updatemenus)
    ) ? el.layout.updatemenus : [];
    const menuGroups = el.querySelectorAll(
      'g.updatemenu-header-group'
    );

    menuGroups.forEach(function(menuGroup, menuIndex) {
      const menu = menus[menuIndex] || {};
      const menuName = String(menu.name || '');
      const toggleMatch = menuName.match(
        /^book-series-toggle-(\d+)$/
      );

      if (!toggleMatch) {
        return;
      }

      const traceIndex = Number.parseInt(toggleMatch[1], 10);
      const trace = (el.data || [])[traceIndex] || {};
      const traceIsVisible =
        trace.visible !== false &&
        trace.visible !== 'legendonly';
      const buttons = menuGroup.querySelectorAll(
        'g.updatemenu-button'
      );

      buttons.forEach(function(button, buttonIndex) {
        const isActive = traceIsVisible && buttonIndex === 0;
        const buttonSpec = (
          Array.isArray(menu.buttons)
            ? menu.buttons[buttonIndex]
            : null
        ) || {};
        const baseLabel = String(buttonSpec.label || '')
          .replace(/^[●○]\s*/, '');
        const textElement = button.querySelector(
          'text.updatemenu-item-text'
        );

        if (textElement && baseLabel) {
          textElement.textContent =
            (isActive ? '● ' : '○ ') + baseLabel;
        }

        const rect = button.querySelector(
          'rect.updatemenu-item-rect'
        );
        const textNodes = button.querySelectorAll(
          'text.updatemenu-item-text, ' +
          'text.updatemenu-item-text tspan'
        );

        button.setAttribute('role', 'button');
        button.setAttribute(
          'aria-pressed',
          isActive ? 'true' : 'false'
        );
        button.setAttribute(
          'data-book-toggle-state',
          isActive ? 'on' : 'off'
        );
        button.style.cursor = 'pointer';

        if (rect) {
          rect.style.setProperty(
            'fill',
            isActive
              ? theme.activeButtonBackground
              : theme.inactiveButtonBackground,
            'important'
          );
          rect.style.setProperty(
            'stroke',
            isActive
              ? theme.activeButtonBorder
              : theme.inactiveButtonBorder,
            'important'
          );
          rect.style.setProperty(
            'stroke-width',
            '1px',
            'important'
          );
          rect.style.setProperty(
            'opacity',
            '1',
            'important'
          );
        }

        textNodes.forEach(function(node) {
          node.style.setProperty(
            'fill',
            isActive
              ? theme.activeButtonText
              : theme.inactiveButtonText,
            'important'
          );
          node.style.setProperty(
            'opacity',
            '1',
            'important'
          );
          node.style.setProperty(
            'font-weight',
            isActive ? '700' : '500',
            'important'
          );
        });
      });
    });
  }

  function applyTheme() {
    if (
      !el ||
      !el.layout ||
      typeof Plotly === 'undefined'
    ) {
      return;
    }

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
    const menuBackground = isDark ? '#2b3035' : '#ffffff';
    const rangeSliderBackground = isDark
      ? '#202428'
      : '#f8f9fa';
    const configuredHeight = requestedChartHeight(styles);
    const toggleTheme = {
      activeButtonBackground: isDark ? '#2563eb' : '#0d6efd',
      activeButtonBorder: isDark ? '#93c5fd' : '#0a58ca',
      activeButtonText: '#ffffff',
      inactiveButtonBackground: menuBackground,
      inactiveButtonBorder: axis,
      inactiveButtonText: text
    };
    el.bookToggleTheme = toggleTheme;

    const layout = {
      paper_bgcolor: background,
      plot_bgcolor: background,
      'font.family': fontFamily,
      'font.color': text,
      'title.font.color': text,
      'legend.font.color': text,
      'legend.bgcolor': 'rgba(0, 0, 0, 0)',
      'hoverlabel.bgcolor': hover,
      'hoverlabel.bordercolor': axis,
      'hoverlabel.font.family': fontFamily,
      'hoverlabel.font.color': text,
      'modebar.bgcolor': 'rgba(0, 0, 0, 0)',
      'modebar.color': text,
      'modebar.activecolor': link
    };

    Object.keys(el.layout)
      .filter(function(name) {
        return /^(xaxis|yaxis)\d*$/.test(name);
      })
      .forEach(function(name) {
        const axisLayout = el.layout[name] || {};

        layout[name + '.color'] = text;
        layout[name + '.tickfont.color'] = text;
        layout[name + '.title.font.color'] = text;
        layout[name + '.gridcolor'] = grid;
        layout[name + '.linecolor'] = axis;
        layout[name + '.zerolinecolor'] = axis;

        if (
          axisLayout.rangeslider &&
          axisLayout.rangeslider.visible
        ) {
          layout[name + '.rangeslider.bgcolor'] =
            rangeSliderBackground;
          layout[name + '.rangeslider.bordercolor'] = axis;
          layout[name + '.rangeslider.borderwidth'] = 1;
        }
      });

    const updateMenus = el.layout.updatemenus || [];
    updateMenus.forEach(function(menu, index) {
      layout['updatemenus[' + index + '].bgcolor'] =
        menuBackground;
      layout['updatemenus[' + index + '].bordercolor'] = axis;
      layout['updatemenus[' + index + '].font.color'] = text;
    });

    if (
      Number.isFinite(configuredHeight) &&
      configuredHeight > 0
    ) {
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

    Promise.resolve(Plotly.relayout(el, layout)).then(function() {
      styleSeriesToggleMenus(toggleTheme);

      window.requestAnimationFrame(function() {
        Plotly.Plots.resize(el);
      });
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

  if (
    el.bookToggleButtonHandler &&
    typeof el.removeListener === 'function'
  ) {
    el.removeListener(
      'plotly_buttonclicked',
      el.bookToggleButtonHandler
    );
    el.removeListener(
      'plotly_restyle',
      el.bookToggleButtonHandler
    );
  }

  el.bookToggleButtonHandler = function() {
    const refreshToggleState = function() {
      if (el.bookToggleTheme) {
        styleSeriesToggleMenus(el.bookToggleTheme);
      }
    };

    window.requestAnimationFrame(refreshToggleState);
    window.setTimeout(refreshToggleState, 60);
  };

  if (typeof el.on === 'function') {
    el.on(
      'plotly_buttonclicked',
      el.bookToggleButtonHandler
    );
    el.on(
      'plotly_restyle',
      el.bookToggleButtonHandler
    );
  }
}
