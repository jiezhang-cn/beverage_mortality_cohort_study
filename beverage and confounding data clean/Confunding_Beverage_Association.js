import React, { useState, useMemo, useRef } from 'react';

const ChordDiagram = () => {
  const [selectedOutcome, setSelectedOutcome] = useState(null);
  const [selectedTerm, setSelectedTerm] = useState(null);
  const chartRef = useRef(null);

  // 改进的SVG导出函数
  const exportToSVG = () => {
    const container = chartRef.current;
    if (!container) return;

    const svgWidth = 1400;
    const svgHeight = 1000;
    
    const svgContent = `
      <svg width="${svgWidth}" height="${svgHeight}" xmlns="http://www.w3.org/2000/svg">
        <defs>
          <style>
            .title { font-family: Arial, sans-serif; font-size: 24px; font-weight: bold; fill: #1f2937; }
            .legend { font-family: Arial, sans-serif; font-size: 14px; fill: #374151; }
            .label-text { font-family: Arial, sans-serif; font-size: 12px; fill: #4b5563; }
            .outcome-text { font-family: Arial, sans-serif; font-size: 14px; font-weight: 500; fill: #374151; }
            .section-title { font-family: Arial, sans-serif; font-size: 14px; font-weight: bold; }
            .negative-title { fill: #dc2626; }
            .positive-title { fill: #059669; }
          </style>
        </defs>
        
        <rect width="100%" height="100%" fill="white"/>
        
        <text x="${svgWidth/2}" y="40" text-anchor="middle" class="title">
          Dietary Intake Associations with Environmental/Lifestyle Factors (Bonferroni corrected)
        </text>
        
        <g transform="translate(${svgWidth/2 - 200}, 70)">
          <rect x="0" y="0" width="2" height="16" fill="black"/>
          <text x="10" y="12" class="legend">-log10(p) &lt; 5</text>
          
          <rect x="120" y="0" width="5" height="16" fill="black"/>
          <text x="135" y="12" class="legend">-log10(p) = 5-10</text>
          
          <rect x="260" y="0" width="8" height="16" fill="black"/>
          <text x="276" y="12" class="legend">-log10(p) = 10-20</text>
          
          <rect x="380" y="0" width="12" height="16" fill="black"/>
          <text x="400" y="12" class="legend">-log10(p) &gt; 20</text>
        </g>
        
        ${generateChordSVG()}
        
        <text x="50" y="${svgHeight - 40}" class="label-text">
          <tspan x="50" dy="0">Line thickness is proportional to -log10(p-value). Bonferroni corrected p-values used.</tspan>
          <tspan x="50" dy="15">Left: negative associations, Right: positive associations.</tspan>
        </text>
      </svg>
    `;

    const blob = new Blob([svgContent], { type: 'image/svg+xml' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'chord-diagram-bonferroni.svg';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
  };

  // 生成图表SVG内容的函数
  const generateChordSVG = () => {
    const startY = 120;
    const chartHeight = 800; // 增加图表高度
    const centerX = 700;
    const leftTermsX = 120;
    const rightTermsX = 1280;

    let svgElements = [];

    // 连接线
    data.forEach((conn, index) => {
      const outcomeIndex = outcomes.findIndex(o => o === conn.outcome);
      const sourcePos = {
        x: centerX,
        y: startY + 100 + outcomeIndex * 80 // 增加outcome节点间距
      };
      
      const isPositive = conn.estimate >= 0;
      const termIndex = isPositive ? 
        termsData.positive.findIndex(t => t.term === conn.term) :
        termsData.negative.findIndex(t => t.term === conn.term);
      
      if (termIndex === -1) return;
      
      const termTotal = isPositive ? termsData.positive.length : termsData.negative.length;
      const targetPos = {
        x: isPositive ? rightTermsX : leftTermsX,
        y: startY + 50 + (termTotal > 1 ? (chartHeight - 100) / (termTotal - 1) * termIndex : 0)
      };

      const outcomeColor = outcomeColors[conn.outcome];
      // 减小线条粗细，使用更小的比例因子
      const logP = -Math.log10(conn.p_value + 1e-300);
      const strokeWidth = Math.max(logP * 0.15, 0.5); // 从0.3减小到0.15
      
      const controlX1 = sourcePos.x + (isPositive ? 250 : -250);
      const controlY1 = sourcePos.y;
      const controlX2 = targetPos.x + (isPositive ? -80 : 80);
      const controlY2 = targetPos.y;

      svgElements.push(`
        <path d="M ${sourcePos.x} ${sourcePos.y} C ${controlX1} ${controlY1}, ${controlX2} ${controlY2}, ${targetPos.x} ${targetPos.y}"
              stroke="${outcomeColor}" stroke-width="${strokeWidth}" fill="none" opacity="0.6"/>
      `);
    });

    // Outcome节点
    outcomes.forEach((outcome, index) => {
      const pos = {
        x: centerX,
        y: startY + 100 + index * 80 // 增加间距
      };
      
      const shortName = outcome.replace('_intake', '').replace('_milk', '').replace('fullfat', 'full').replace('lowfat', 'low');
      
      svgElements.push(`
        <circle cx="${pos.x}" cy="${pos.y}" r="25" fill="${outcomeColors[outcome]}" stroke="white" stroke-width="2"/>
        <text x="${pos.x}" y="${pos.y + 4}" text-anchor="middle" fill="white" font-size="10" font-weight="bold">
          ${shortName}
        </text>
        <text x="${pos.x + 40}" y="${pos.y + 5}" class="outcome-text">
          ${outcome.replace('_', ' ')}
        </text>
      `);
    });

    // 左侧标签（负相关）
    svgElements.push(`
      <text x="50" y="${startY + 30}" class="section-title negative-title">
        Negative Association (estimate &lt; 0)
      </text>
    `);
    
    termsData.negative.forEach((termData, index) => {
      const pos = {
        x: leftTermsX,
        y: startY + 50 + (termsData.negative.length > 1 ? (chartHeight - 100) / (termsData.negative.length - 1) * index : 0)
      };
      
      svgElements.push(`
        <circle cx="${pos.x}" cy="${pos.y}" r="3" fill="#DC2626"/>
        <text x="${pos.x - 10}" y="${pos.y + 3}" text-anchor="end" class="label-text">
          ${termData.term.length > 25 ? termData.term.substring(0, 22) + '...' : termData.term}
        </text>
      `);
    });

    // 右侧标签（正相关）
    svgElements.push(`
      <text x="${rightTermsX + 50}" y="${startY + 30}" class="section-title positive-title">
        Positive Association (estimate &gt; 0)
      </text>
    `);
    
    termsData.positive.forEach((termData, index) => {
      const pos = {
        x: rightTermsX,
        y: startY + 50 + (termsData.positive.length > 1 ? (chartHeight - 100) / (termsData.positive.length - 1) * index : 0)
      };
      
      svgElements.push(`
        <circle cx="${pos.x}" cy="${pos.y}" r="3" fill="#059669"/>
        <text x="${pos.x + 10}" y="${pos.y + 3}" text-anchor="start" class="label-text">
          ${termData.term.length > 25 ? termData.term.substring(0, 22) + '...' : termData.term}
        </text>
      `);
    });

    return svgElements.join('\n');
  };

  const exportToPDF = () => {
    const interactiveElements = chartRef.current.querySelectorAll('.no-print');
    interactiveElements.forEach(el => el.style.display = 'none');
    
    const originalStyle = document.body.style.cssText;
    document.body.style.cssText = `
      margin: 0;
      padding: 0;
      -webkit-print-color-adjust: exact;
      color-adjust: exact;
    `;
    
    window.print();
    
    setTimeout(() => {
      document.body.style.cssText = originalStyle;
      interactiveElements.forEach(el => el.style.display = '');
    }, 1000);
  };

  // Bonferroni校正后的183行显著数据
  const data = useMemo(() => [
    { term: 'age', estimate: -0.00980, outcome: 'SSB_intake', p_value: 7.83e-254, p_value_bonferroni: 3.07e-251 },
    { term: 'sex', estimate: 0.115, outcome: 'SSB_intake', p_value: 6.09e-206, p_value_bonferroni: 2.39e-203 },
    { term: 'ethnic3', estimate: -0.0619, outcome: 'SSB_intake', p_value: 9.09e-6, p_value_bonferroni: 3.56e-3 },
    { term: 'ethnic4', estimate: 0.210, outcome: 'SSB_intake', p_value: 1.11e-36, p_value_bonferroni: 4.34e-34 },
    { term: 'chesse_intake2', estimate: -0.0585, outcome: 'SSB_intake', p_value: 4.25e-6, p_value_bonferroni: 1.67e-3 },
    { term: 'chesse_intake3', estimate: -0.0523, outcome: 'SSB_intake', p_value: 3.25e-5, p_value_bonferroni: 1.28e-2 },
    { term: 'nap_frequency2', estimate: 0.0274, outcome: 'SSB_intake', p_value: 8.09e-13, p_value_bonferroni: 3.17e-10 },
    { term: 'nap_frequency3', estimate: 0.0407, outcome: 'SSB_intake', p_value: 4.07e-6, p_value_bonferroni: 1.60e-3 },
    { term: 'household_income4', estimate: -0.0465, outcome: 'SSB_intake', p_value: 3.62e-11, p_value_bonferroni: 1.42e-8 },
    { term: 'household_income5', estimate: -0.0759, outcome: 'SSB_intake', p_value: 2.62e-16, p_value_bonferroni: 1.03e-13 },
    { term: 'employed2', estimate: -0.0317, outcome: 'SSB_intake', p_value: 4.51e-12, p_value_bonferroni: 1.77e-9 },
    { term: 'education_years6', estimate: -0.0522, outcome: 'SSB_intake', p_value: 4.10e-13, p_value_bonferroni: 1.61e-10 },
    { term: 'smoking_status2', estimate: -0.0335, outcome: 'SSB_intake', p_value: 6.21e-12, p_value_bonferroni: 2.43e-9 },
    { term: 'smoking_status3', estimate: -0.0437, outcome: 'SSB_intake', p_value: 3.27e-8, p_value_bonferroni: 1.28e-5 },
    
    { term: 'age', estimate: -0.0101, outcome: 'ASB_intake', p_value: 0, p_value_bonferroni: 0 },
    { term: 'sex', estimate: -0.0140, outcome: 'ASB_intake', p_value: 1.36e-5, p_value_bonferroni: 5.35e-3 },
    { term: 'ethnic2', estimate: -0.0812, outcome: 'ASB_intake', p_value: 3.95e-5, p_value_bonferroni: 1.55e-2 },
    { term: 'ethnic3', estimate: -0.109, outcome: 'ASB_intake', p_value: 8.03e-20, p_value_bonferroni: 3.15e-17 },
    { term: 'ethnic4', estimate: -0.0788, outcome: 'ASB_intake', p_value: 2.84e-8, p_value_bonferroni: 1.11e-5 },
    { term: 'fed_up_feeling', estimate: 0.0236, outcome: 'ASB_intake', p_value: 2.94e-11, p_value_bonferroni: 1.15e-8 },
    { term: 'pack_years_smoking', estimate: 0.00125, outcome: 'ASB_intake', p_value: 4.37e-19, p_value_bonferroni: 1.71e-16 },
    { term: 'chesse_intake4', estimate: -0.0529, outcome: 'ASB_intake', p_value: 4.49e-7, p_value_bonferroni: 1.76e-4 },
    { term: 'chesse_intake5', estimate: -0.0678, outcome: 'ASB_intake', p_value: 1.40e-9, p_value_bonferroni: 5.47e-7 },
    { term: 'chesse_intake6', estimate: -0.0481, outcome: 'ASB_intake', p_value: 1.20e-4, p_value_bonferroni: 4.72e-2 },
    { term: 'nap_frequency2', estimate: 0.0185, outcome: 'ASB_intake', p_value: 1.60e-8, p_value_bonferroni: 6.29e-6 },
    { term: 'nap_frequency3', estimate: 0.0411, outcome: 'ASB_intake', p_value: 5.78e-8, p_value_bonferroni: 2.26e-5 },
    { term: 'frequency_tiredness3', estimate: 0.0283, outcome: 'ASB_intake', p_value: 1.26e-4, p_value_bonferroni: 4.95e-2 },
    { term: 'frequency_tiredness4', estimate: 0.0575, outcome: 'ASB_intake', p_value: 1.79e-13, p_value_bonferroni: 7.03e-11 },
    { term: 'physical_activity_group2', estimate: -0.0174, outcome: 'ASB_intake', p_value: 4.59e-5, p_value_bonferroni: 1.80e-2 },
    { term: 'employed2', estimate: -0.0315, outcome: 'ASB_intake', p_value: 1.06e-15, p_value_bonferroni: 4.15e-13 },
    { term: 'sleep_duration_group2', estimate: 0.0241, outcome: 'ASB_intake', p_value: 4.99e-11, p_value_bonferroni: 1.95e-8 },
    { term: 'sleep_duration_group3', estimate: 0.0778, outcome: 'ASB_intake', p_value: 2.35e-8, p_value_bonferroni: 9.23e-6 },
    { term: 'plumper_than102', estimate: 0.103, outcome: 'ASB_intake', p_value: 7.04e-138, p_value_bonferroni: 2.76e-135 },
    { term: 'renting_from_council_vs_own2', estimate: 0.0290, outcome: 'ASB_intake', p_value: 3.08e-5, p_value_bonferroni: 1.21e-2 },
    { term: 'education_years3', estimate: -0.0327, outcome: 'ASB_intake', p_value: 6.16e-5, p_value_bonferroni: 2.42e-2 },
    { term: 'education_years6', estimate: -0.0595, outcome: 'ASB_intake', p_value: 4.37e-22, p_value_bonferroni: 1.71e-19 },
    { term: 'smoking_status3', estimate: -0.0388, outcome: 'ASB_intake', p_value: 1.02e-8, p_value_bonferroni: 4.01e-6 },
    
    { term: 'age', estimate: 0.00243, outcome: 'NJ_intake', p_value: 3.63e-21, p_value_bonferroni: 1.42e-18 },
    { term: 'sex', estimate: 0.0634, outcome: 'NJ_intake', p_value: 3.35e-79, p_value_bonferroni: 1.31e-76 },
    { term: 'ethnic3', estimate: -0.0781, outcome: 'NJ_intake', p_value: 3.90e-10, p_value_bonferroni: 1.53e-7 },
    { term: 'ethnic4', estimate: 0.206, outcome: 'NJ_intake', p_value: 5.07e-44, p_value_bonferroni: 1.99e-41 },
    { term: 'ethnic5', estimate: 0.125, outcome: 'NJ_intake', p_value: 3.28e-11, p_value_bonferroni: 1.28e-8 },
    { term: 'fed_up_feeling', estimate: -0.0175, outcome: 'NJ_intake', p_value: 2.13e-6, p_value_bonferroni: 8.35e-4 },
    { term: 'pack_years_smoking', estimate: -0.00107, outcome: 'NJ_intake', p_value: 2.31e-13, p_value_bonferroni: 9.05e-11 },
    { term: 'chesse_intake5', estimate: 0.0617, outcome: 'NJ_intake', p_value: 1.32e-7, p_value_bonferroni: 5.16e-5 },
    { term: 'chesse_intake6', estimate: 0.0652, outcome: 'NJ_intake', p_value: 6.19e-7, p_value_bonferroni: 2.43e-4 },
    { term: 'frequency_tiredness2', estimate: -0.0172, outcome: 'NJ_intake', p_value: 1.80e-6, p_value_bonferroni: 7.07e-4 },
    { term: 'ease_skin_tanning3', estimate: 0.0249, outcome: 'NJ_intake', p_value: 4.08e-7, p_value_bonferroni: 1.60e-4 },
    { term: 'ease_skin_tanning4', estimate: 0.0203, outcome: 'NJ_intake', p_value: 1.16e-4, p_value_bonferroni: 4.56e-2 },
    { term: 'household_income3', estimate: 0.0255, outcome: 'NJ_intake', p_value: 5.33e-6, p_value_bonferroni: 2.09e-3 },
    { term: 'household_income4', estimate: 0.0378, outcome: 'NJ_intake', p_value: 1.92e-9, p_value_bonferroni: 7.52e-7 },
    { term: 'household_income5', estimate: 0.0655, outcome: 'NJ_intake', p_value: 2.70e-15, p_value_bonferroni: 1.06e-12 },
    { term: 'live_with_partner2', estimate: -0.0285, outcome: 'NJ_intake', p_value: 6.52e-12, p_value_bonferroni: 2.56e-9 },
    { term: 'physical_activity_group2', estimate: 0.0213, outcome: 'NJ_intake', p_value: 1.83e-6, p_value_bonferroni: 7.17e-4 },
    { term: 'physical_activity_group3', estimate: 0.0276, outcome: 'NJ_intake', p_value: 3.40e-9, p_value_bonferroni: 1.33e-6 },
    { term: 'employed2', estimate: -0.0286, outcome: 'NJ_intake', p_value: 3.05e-12, p_value_bonferroni: 1.20e-9 },
    { term: 'living_flat_vs_house2', estimate: 0.0322, outcome: 'NJ_intake', p_value: 3.48e-7, p_value_bonferroni: 1.36e-4 },
    { term: 'financial_diffculty2', estimate: -0.0275, outcome: 'NJ_intake', p_value: 3.52e-5, p_value_bonferroni: 1.38e-2 },
    { term: 'plumper_than102', estimate: -0.0574, outcome: 'NJ_intake', p_value: 2.60e-40, p_value_bonferroni: 1.02e-37 },
    { term: 'education_years2', estimate: 0.0454, outcome: 'NJ_intake', p_value: 4.24e-11, p_value_bonferroni: 1.66e-8 },
    { term: 'education_years3', estimate: 0.105, outcome: 'NJ_intake', p_value: 4.91e-35, p_value_bonferroni: 1.92e-32 },
    { term: 'education_years4', estimate: 0.102, outcome: 'NJ_intake', p_value: 1.62e-45, p_value_bonferroni: 6.36e-43 },
    { term: 'education_years5', estimate: 0.0617, outcome: 'NJ_intake', p_value: 1.59e-18, p_value_bonferroni: 6.22e-16 },
    { term: 'education_years6', estimate: 0.173, outcome: 'NJ_intake', p_value: 2.53e-159, p_value_bonferroni: 9.91e-157 },
    { term: 'smoking_status2', estimate: -0.0325, outcome: 'NJ_intake', p_value: 9.26e-14, p_value_bonferroni: 3.63e-11 },
    { term: 'smoking_status3', estimate: -0.0602, outcome: 'NJ_intake', p_value: 1.91e-17, p_value_bonferroni: 7.49e-15 },
    
    { term: 'age', estimate: 0.0163, outcome: 'lowfat_milk_intake', p_value: 2.47e-66, p_value_bonferroni: 9.68e-64 },
    { term: 'ethnic2', estimate: -0.648, outcome: 'lowfat_milk_intake', p_value: 1.74e-17, p_value_bonferroni: 6.81e-15 },
    { term: 'ethnic3', estimate: -1.12, outcome: 'lowfat_milk_intake', p_value: 2.34e-131, p_value_bonferroni: 9.16e-129 },
    { term: 'ethnic4', estimate: -1.70, outcome: 'lowfat_milk_intake', p_value: 2.35e-211, p_value_bonferroni: 9.22e-209 },
    { term: 'ethnic5', estimate: -1.05, outcome: 'lowfat_milk_intake', p_value: 4.92e-52, p_value_bonferroni: 1.93e-49 },
    { term: 'fed_up_feeling', estimate: 0.0883, outcome: 'lowfat_milk_intake', p_value: 9.64e-11, p_value_bonferroni: 3.78e-8 },
    { term: 'tdi', estimate: -0.0425, outcome: 'lowfat_milk_intake', p_value: 3.78e-71, p_value_bonferroni: 1.48e-68 },
    { term: 'chesse_intake4', estimate: 0.170, outcome: 'lowfat_milk_intake', p_value: 2.55e-5, p_value_bonferroni: 1.00e-2 },
    { term: 'frequency_unenthusiasm2', estimate: -0.0790, outcome: 'lowfat_milk_intake', p_value: 1.29e-5, p_value_bonferroni: 5.07e-3 },
    { term: 'frequency_tiredness2', estimate: -0.0538, outcome: 'lowfat_milk_intake', p_value: 4.98e-5, p_value_bonferroni: 1.95e-2 },
    { term: 'frequency_tiredness4', estimate: -0.154, outcome: 'lowfat_milk_intake', p_value: 2.87e-7, p_value_bonferroni: 1.13e-4 },
    { term: 'ease_skin_tanning2', estimate: 0.0794, outcome: 'lowfat_milk_intake', p_value: 5.33e-7, p_value_bonferroni: 2.09e-4 },
    { term: 'ease_skin_tanning3', estimate: 0.130, outcome: 'lowfat_milk_intake', p_value: 7.28e-13, p_value_bonferroni: 2.85e-10 },
    { term: 'ease_skin_tanning4', estimate: 0.121, outcome: 'lowfat_milk_intake', p_value: 4.68e-10, p_value_bonferroni: 1.84e-7 },
    { term: 'household_income4', estimate: -0.144, outcome: 'lowfat_milk_intake', p_value: 5.59e-10, p_value_bonferroni: 2.19e-7 },
    { term: 'household_income5', estimate: -0.256, outcome: 'lowfat_milk_intake', p_value: 5.92e-17, p_value_bonferroni: 2.32e-14 },
    { term: 'live_with_partner2', estimate: 0.169, outcome: 'lowfat_milk_intake', p_value: 2.29e-28, p_value_bonferroni: 8.97e-26 },
    { term: 'shorter_than102', estimate: -0.0929, outcome: 'lowfat_milk_intake', p_value: 1.55e-10, p_value_bonferroni: 6.07e-8 },
    { term: 'living_flat_vs_house2', estimate: -0.367, outcome: 'lowfat_milk_intake', p_value: 6.10e-56, p_value_bonferroni: 2.39e-53 },
    { term: 'financial_diffculty2', estimate: -0.173, outcome: 'lowfat_milk_intake', p_value: 1.69e-12, p_value_bonferroni: 6.61e-10 },
    { term: 'sleep_duration_group2', estimate: -0.0615, outcome: 'lowfat_milk_intake', p_value: 1.37e-5, p_value_bonferroni: 5.36e-3 },
    { term: 'plumper_than102', estimate: 0.0979, outcome: 'lowfat_milk_intake', p_value: 7.97e-10, p_value_bonferroni: 3.12e-7 },
    { term: 'education_years3', estimate: -0.199, outcome: 'lowfat_milk_intake', p_value: 2.15e-10, p_value_bonferroni: 8.41e-8 },
    { term: 'education_years6', estimate: -0.126, outcome: 'lowfat_milk_intake', p_value: 1.07e-7, p_value_bonferroni: 4.19e-5 },
    { term: 'smoking_status2', estimate: -0.112, outcome: 'lowfat_milk_intake', p_value: 3.05e-12, p_value_bonferroni: 1.20e-9 },
    
    { term: 'sex', estimate: 0.132, outcome: 'fullfat_milk_intake', p_value: 3.33e-167, p_value_bonferroni: 1.30e-164 },
    { term: 'tdi', estimate: 0.00936, outcome: 'fullfat_milk_intake', p_value: 1.74e-24, p_value_bonferroni: 6.82e-22 },
    { term: 'pack_years_smoking', estimate: 0.00169, outcome: 'fullfat_milk_intake', p_value: 3.03e-16, p_value_bonferroni: 1.19e-13 },
    { term: 'chesse_intake5', estimate: 0.0898, outcome: 'fullfat_milk_intake', p_value: 6.08e-8, p_value_bonferroni: 2.38e-5 },
    { term: 'chesse_intake6', estimate: 0.165, outcome: 'fullfat_milk_intake', p_value: 7.00e-19, p_value_bonferroni: 2.74e-16 },
    { term: 'household_income2', estimate: -0.0575, outcome: 'fullfat_milk_intake', p_value: 1.41e-14, p_value_bonferroni: 5.53e-12 },
    { term: 'household_income3', estimate: -0.0867, outcome: 'fullfat_milk_intake', p_value: 1.04e-27, p_value_bonferroni: 4.09e-25 },
    { term: 'household_income4', estimate: -0.118, outcome: 'fullfat_milk_intake', p_value: 2.76e-40, p_value_bonferroni: 1.08e-37 },
    { term: 'household_income5', estimate: -0.141, outcome: 'fullfat_milk_intake', p_value: 2.32e-33, p_value_bonferroni: 9.09e-31 },
    { term: 'live_with_partner2', estimate: -0.0364, outcome: 'fullfat_milk_intake', p_value: 6.07e-10, p_value_bonferroni: 2.38e-7 },
    { term: 'physical_activity_group3', estimate: 0.0312, outcome: 'fullfat_milk_intake', p_value: 2.34e-6, p_value_bonferroni: 9.17e-4 },
    { term: 'plumper_than102', estimate: -0.0524, outcome: 'fullfat_milk_intake', p_value: 1.10e-17, p_value_bonferroni: 4.32e-15 },
    { term: 'use_open_fire2', estimate: 0.333, outcome: 'fullfat_milk_intake', p_value: 1.50e-9, p_value_bonferroni: 5.88e-7 },
    { term: 'use_gym2', estimate: -0.0687, outcome: 'fullfat_milk_intake', p_value: 2.90e-42, p_value_bonferroni: 1.14e-39 },
    { term: 'smoking_status2', estimate: -0.0589, outcome: 'fullfat_milk_intake', p_value: 1.38e-21, p_value_bonferroni: 5.41e-19 },
    { term: 'smoking_status3', estimate: 0.168, outcome: 'fullfat_milk_intake', p_value: 6.25e-63, p_value_bonferroni: 2.45e-60 },
    
    { term: 'age', estimate: -0.0123, outcome: 'plain_water_intake', p_value: 2.99e-103, p_value_bonferroni: 1.17e-100 },
    { term: 'sex', estimate: -0.341, outcome: 'plain_water_intake', p_value: 0, p_value_bonferroni: 0 },
    { term: 'ethnic2', estimate: 0.192, outcome: 'plain_water_intake', p_value: 2.62e-5, p_value_bonferroni: 1.03e-2 },
    { term: 'ethnic3', estimate: 1.04, outcome: 'plain_water_intake', p_value: 7.11e-312, p_value_bonferroni: 2.79e-309 },
    { term: 'ethnic4', estimate: 0.444, outcome: 'plain_water_intake', p_value: 8.52e-42, p_value_bonferroni: 3.34e-39 },
    { term: 'ethnic5', estimate: 0.558, outcome: 'plain_water_intake', p_value: 5.66e-41, p_value_bonferroni: 2.22e-38 },
    { term: 'fed_up_feeling', estimate: -0.0795, outcome: 'plain_water_intake', p_value: 2.61e-22, p_value_bonferroni: 1.02e-19 },
    { term: 'tdi', estimate: 0.0180, outcome: 'plain_water_intake', p_value: 2.30e-36, p_value_bonferroni: 9.01e-34 },
    { term: 'pack_years_smoking', estimate: -0.00186, outcome: 'plain_water_intake', p_value: 7.88e-9, p_value_bonferroni: 3.09e-6 },
    { term: 'ease_skin_tanning2', estimate: -0.0376, outcome: 'plain_water_intake', p_value: 7.61e-5, p_value_bonferroni: 2.98e-2 },
    { term: 'household_income2', estimate: 0.0628, outcome: 'plain_water_intake', p_value: 7.10e-8, p_value_bonferroni: 2.78e-5 },
    { term: 'household_income3', estimate: 0.107, outcome: 'plain_water_intake', p_value: 7.70e-18, p_value_bonferroni: 3.02e-15 },
    { term: 'household_income4', estimate: 0.190, outcome: 'plain_water_intake', p_value: 1.55e-42, p_value_bonferroni: 6.08e-40 },
    { term: 'household_income5', estimate: 0.357, outcome: 'plain_water_intake', p_value: 3.07e-84, p_value_bonferroni: 1.20e-81 },
    { term: 'live_with_partner2', estimate: -0.0548, outcome: 'plain_water_intake', p_value: 2.33e-9, p_value_bonferroni: 9.13e-7 },
    { term: 'physical_activity_group2', estimate: 0.150, outcome: 'plain_water_intake', p_value: 2.78e-52, p_value_bonferroni: 1.09e-49 },
    { term: 'physical_activity_group3', estimate: 0.320, outcome: 'plain_water_intake', p_value: 2.86e-210, p_value_bonferroni: 1.12e-207 },
    { term: 'shorter_than102', estimate: -0.0838, outcome: 'plain_water_intake', p_value: 6.13e-22, p_value_bonferroni: 2.40e-19 },
    { term: 'living_flat_vs_house2', estimate: 0.230, outcome: 'plain_water_intake', p_value: 6.54e-61, p_value_bonferroni: 2.56e-58 },
    { term: 'education_years3', estimate: 0.158, outcome: 'plain_water_intake', p_value: 5.41e-17, p_value_bonferroni: 2.12e-14 },
    { term: 'education_years4', estimate: 0.122, outcome: 'plain_water_intake', p_value: 1.93e-14, p_value_bonferroni: 7.58e-12 },
    { term: 'education_years6', estimate: 0.138, outcome: 'plain_water_intake', p_value: 2.42e-22, p_value_bonferroni: 9.47e-20 },
    { term: 'use_gym2', estimate: 0.159, outcome: 'plain_water_intake', p_value: 2.80e-90, p_value_bonferroni: 1.10e-87 },
    { term: 'smoking_status2', estimate: 0.101, outcome: 'plain_water_intake', p_value: 1.35e-25, p_value_bonferroni: 5.31e-23 },
    { term: 'smoking_status3', estimate: -0.133, outcome: 'plain_water_intake', p_value: 2.40e-17, p_value_bonferroni: 9.40e-15 },
    
    { term: 'age', estimate: 0.0103, outcome: 'coffee_intake', p_value: 5.81e-64, p_value_bonferroni: 2.28e-61 },
    { term: 'sex', estimate: 0.120, outcome: 'coffee_intake', p_value: 9.72e-51, p_value_bonferroni: 3.81e-48 },
    { term: 'ethnic2', estimate: -0.359, outcome: 'coffee_intake', p_value: 2.41e-13, p_value_bonferroni: 9.43e-11 },
    { term: 'ethnic3', estimate: -0.904, outcome: 'coffee_intake', p_value: 4.72e-204, p_value_bonferroni: 1.85e-201 },
    { term: 'ethnic4', estimate: -0.889, outcome: 'coffee_intake', p_value: 1.05e-140, p_value_bonferroni: 4.13e-138 },
    { term: 'ethnic5', estimate: -0.427, outcome: 'coffee_intake', p_value: 1.06e-21, p_value_bonferroni: 4.15e-19 },
    { term: 'tdi', estimate: -0.0112, outcome: 'coffee_intake', p_value: 3.46e-13, p_value_bonferroni: 1.36e-10 },
    { term: 'pack_years_smoking', estimate: 0.00450, outcome: 'coffee_intake', p_value: 1.44e-38, p_value_bonferroni: 5.65e-36 },
    { term: 'chesse_intake2', estimate: 0.143, outcome: 'coffee_intake', p_value: 1.11e-7, p_value_bonferroni: 4.35e-5 },
    { term: 'chesse_intake3', estimate: 0.168, outcome: 'coffee_intake', p_value: 3.44e-10, p_value_bonferroni: 1.35e-7 },
    { term: 'chesse_intake4', estimate: 0.244, outcome: 'coffee_intake', p_value: 5.78e-21, p_value_bonferroni: 2.26e-18 },
    { term: 'chesse_intake5', estimate: 0.342, outcome: 'coffee_intake', p_value: 7.76e-35, p_value_bonferroni: 3.04e-32 },
    { term: 'chesse_intake6', estimate: 0.414, outcome: 'coffee_intake', p_value: 1.55e-40, p_value_bonferroni: 6.06e-38 },
    { term: 'nap_frequency2', estimate: -0.0324, outcome: 'coffee_intake', p_value: 6.71e-5, p_value_bonferroni: 2.63e-2 },
    { term: 'frequency_tiredness2', estimate: -0.0656, outcome: 'coffee_intake', p_value: 1.62e-14, p_value_bonferroni: 6.36e-12 },
    { term: 'frequency_tiredness3', estimate: -0.0986, outcome: 'coffee_intake', p_value: 7.19e-8, p_value_bonferroni: 2.82e-5 },
    { term: 'frequency_tiredness4', estimate: -0.118, outcome: 'coffee_intake', p_value: 1.26e-9, p_value_bonferroni: 4.94e-7 },
    { term: 'household_income2', estimate: 0.0481, outcome: 'coffee_intake', p_value: 1.22e-4, p_value_bonferroni: 4.78e-2 },
    { term: 'household_income3', estimate: 0.0585, outcome: 'coffee_intake', p_value: 1.11e-5, p_value_bonferroni: 4.36e-3 },
    { term: 'household_income5', estimate: 0.117, outcome: 'coffee_intake', p_value: 3.01e-9, p_value_bonferroni: 1.18e-6 },
    { term: 'live_with_partner2', estimate: 0.0450, outcome: 'coffee_intake', p_value: 5.01e-6, p_value_bonferroni: 1.96e-3 },
    { term: 'shorter_than102', estimate: -0.0618, outcome: 'coffee_intake', p_value: 3.78e-11, p_value_bonferroni: 1.48e-8 },
    { term: 'sleep_duration_group2', estimate: 0.0710, outcome: 'coffee_intake', p_value: 6.52e-15, p_value_bonferroni: 2.55e-12 },
    { term: 'plumper_than102', estimate: 0.233, outcome: 'coffee_intake', p_value: 1.85e-114, p_value_bonferroni: 7.26e-112 },
    { term: 'education_years2', estimate: 0.0736, outcome: 'coffee_intake', p_value: 6.59e-6, p_value_bonferroni: 2.58e-3 },
    { term: 'education_years4', estimate: 0.134, outcome: 'coffee_intake', p_value: 4.64e-15, p_value_bonferroni: 1.82e-12 },
    { term: 'education_years5', estimate: 0.0873, outcome: 'coffee_intake', p_value: 1.67e-7, p_value_bonferroni: 6.55e-5 },
    { term: 'education_years6', estimate: 0.190, outcome: 'coffee_intake', p_value: 1.29e-35, p_value_bonferroni: 5.04e-33 },
    { term: 'smoking_status2', estimate: 0.0410, outcome: 'coffee_intake', p_value: 7.40e-5, p_value_bonferroni: 2.90e-2 },
    { term: 'smoking_status3', estimate: 0.419, outcome: 'coffee_intake', p_value: 4.50e-137, p_value_bonferroni: 1.76e-134 },
    
    { term: 'age', estimate: 0.0131, outcome: 'tea_intake', p_value: 9.24e-68, p_value_bonferroni: 3.62e-65 },
    { term: 'sex', estimate: -0.251, outcome: 'tea_intake', p_value: 3.18e-144, p_value_bonferroni: 1.25e-141 },
    { term: 'ethnic3', estimate: -0.375, outcome: 'tea_intake', p_value: 7.33e-25, p_value_bonferroni: 2.87e-22 },
    { term: 'ethnic4', estimate: -0.863, outcome: 'tea_intake', p_value: 2.07e-88, p_value_bonferroni: 8.12e-86 },
    { term: 'ethnic5', estimate: -0.314, outcome: 'tea_intake', p_value: 1.02e-8, p_value_bonferroni: 4.01e-6 },
    { term: 'tdi', estimate: -0.00856, outcome: 'tea_intake', p_value: 5.75e-6, p_value_bonferroni: 2.25e-3 },
    { term: 'household_income2', estimate: -0.0847, outcome: 'tea_intake', p_value: 3.55e-8, p_value_bonferroni: 1.39e-5 },
    { term: 'household_income3', estimate: -0.145, outcome: 'tea_intake', p_value: 6.24e-19, p_value_bonferroni: 2.45e-16 },
    { term: 'household_income4', estimate: -0.226, outcome: 'tea_intake', p_value: 7.22e-35, p_value_bonferroni: 2.83e-32 },
    { term: 'household_income5', estimate: -0.392, outcome: 'tea_intake', p_value: 3.84e-59, p_value_bonferroni: 1.51e-56 },
    { term: 'live_with_partner2', estimate: 0.132, outcome: 'tea_intake', p_value: 1.81e-27, p_value_bonferroni: 7.10e-25 },
    { term: 'physical_activity_group2', estimate: 0.111, outcome: 'tea_intake', p_value: 1.33e-17, p_value_bonferroni: 5.20e-15 },
    { term: 'physical_activity_group3', estimate: 0.168, outcome: 'tea_intake', p_value: 5.60e-35, p_value_bonferroni: 2.19e-32 },
    { term: 'employed2', estimate: 0.0466, outcome: 'tea_intake', p_value: 9.97e-5, p_value_bonferroni: 3.91e-2 },
    { term: 'living_flat_vs_house2', estimate: -0.192, outcome: 'tea_intake', p_value: 2.19e-25, p_value_bonferroni: 8.59e-23 },
    { term: 'financial_diffculty2', estimate: -0.131, outcome: 'tea_intake', p_value: 1.35e-11, p_value_bonferroni: 5.29e-9 },
    { term: 'sleep_duration_group2', estimate: -0.0717, outcome: 'tea_intake', p_value: 1.45e-10, p_value_bonferroni: 5.70e-8 },
    { term: 'plumper_than102', estimate: -0.140, outcome: 'tea_intake', p_value: 1.77e-28, p_value_bonferroni: 6.96e-26 },
    { term: 'education_years2', estimate: -0.122, outcome: 'tea_intake', p_value: 1.07e-9, p_value_bonferroni: 4.21e-7 },
    { term: 'use_gym2', estimate: -0.0687, outcome: 'tea_intake', p_value: 3.62e-11, p_value_bonferroni: 1.42e-8 },
    { term: 'smoking_status3', estimate: -0.174, outcome: 'tea_intake', p_value: 3.37e-17, p_value_bonferroni: 1.32e-14 }
  ], []);

  const outcomes = [
    'SSB_intake',
    'ASB_intake', 
    'NJ_intake',
    'lowfat_milk_intake',
    'fullfat_milk_intake',
    'plain_water_intake',
    'coffee_intake',
    'tea_intake'
  ];

  const outcomeColors = {
    'SSB_intake': '#FF6B6B',
    'ASB_intake': '#4ECDC4',
    'NJ_intake': '#45B7D1',
    'lowfat_milk_intake': '#96CEB4',
    'fullfat_milk_intake': '#FFEAA7',
    'plain_water_intake': '#DDA0DD',
    'coffee_intake': '#98D8C8',
    'tea_intake': '#F7DC6F'
  };

  const allTerms = useMemo(() => {
    const terms = [...new Set(data.map(d => d.term))];
    return terms.sort();
  }, [data]);

  const termsData = useMemo(() => {
    const positiveTerms = [];
    const negativeTerms = [];
    
    allTerms.forEach(term => {
      const termData = data.filter(d => d.term === term);
      const avgEstimate = termData.reduce((sum, d) => sum + d.estimate, 0) / termData.length;
      
      if (avgEstimate >= 0) {
        positiveTerms.push({ term, avgEstimate });
      } else {
        negativeTerms.push({ term, avgEstimate });
      }
    });
    
    return {
      positive: positiveTerms.sort((a, b) => b.avgEstimate - a.avgEstimate),
      negative: negativeTerms.sort((a, b) => a.avgEstimate - b.avgEstimate)
    };
  }, [allTerms, data]);

  const svgWidth = 1400;
  const svgHeight = 900; // 增加高度
  const centerX = svgWidth / 2;
  const leftTermsX = 120;
  const rightTermsX = svgWidth - 120;

  const getOutcomePosition = (index) => {
    const startY = 200; // 增加起始位置
    const spacing = 80; // 增加间距
    return {
      x: centerX,
      y: startY + index * spacing
    };
  };

  const getTermPosition = (index, total, isPositive) => {
    const startY = 150; // 增加起始位置
    const endY = svgHeight - 100;
    const spacing = total > 1 ? (endY - startY) / (total - 1) : 0;
    return {
      x: isPositive ? rightTermsX : leftTermsX,
      y: startY + index * spacing
    };
  };

  return (
    <div className="w-full max-w-7xl mx-auto p-6 bg-white print:p-2" ref={chartRef}>
      <style jsx>{`
        @media print {
          body { 
            margin: 0; 
            -webkit-print-color-adjust: exact;
            color-adjust: exact;
          }
          .no-print { display: none !important; }
          .print-break { page-break-after: always; }
          svg { 
            -webkit-print-color-adjust: exact;
            color-adjust: exact;
          }
        }
      `}</style>
      
      <div className="mb-4 flex justify-center space-x-4 no-print">
        <button
          onClick={exportToPDF}
          className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600 transition-colors"
        >
          导出为PDF
        </button>
        <button
          onClick={exportToSVG}
          className="px-4 py-2 bg-green-500 text-white rounded hover:bg-green-600 transition-colors"
        >
          导出为完整SVG
        </button>
      </div>

      <h2 className="text-2xl font-bold text-center mb-2 print:text-xl">
        Dietary Intake Associations with Environmental/Lifestyle Factors (Bonferroni corrected)
      </h2>
      
      <div className="mb-4 flex justify-center space-x-6 print:mb-2">
        <div className="flex items-center space-x-2">
          <div className="w-1 h-4 bg-black"></div>
          <span className="text-sm">-log10(p) &lt; 5</span>
        </div>
        <div className="flex items-center space-x-2">
          <div className="w-2 h-4 bg-black"></div>
          <span className="text-sm">-log10(p) = 5-10</span>
        </div>
        <div className="flex items-center space-x-2">
          <div className="w-4 h-4 bg-black"></div>
          <span className="text-sm">-log10(p) = 10-20</span>
        </div>
        <div className="flex items-center space-x-2">
          <div className="w-6 h-4 bg-black"></div>
          <span className="text-sm">-log10(p) &gt; 20</span>
        </div>
      </div>

      <svg width={svgWidth} height={svgHeight} className="border border-gray-200 print:border-black" style={{colorAdjust: 'exact', WebkitPrintColorAdjust: 'exact'}}>
        {data.map((conn, index) => {
          const outcomeIndex = outcomes.findIndex(o => o === conn.outcome);
          const sourcePos = getOutcomePosition(outcomeIndex);
          
          const isPositive = conn.estimate >= 0;
          const termIndex = isPositive ? 
            termsData.positive.findIndex(t => t.term === conn.term) :
            termsData.negative.findIndex(t => t.term === conn.term);
          
          if (termIndex === -1) return null;
          
          const termTotal = isPositive ? termsData.positive.length : termsData.negative.length;
          const targetPos = getTermPosition(termIndex, termTotal, isPositive);

          const isHighlighted = 
            (selectedOutcome === null || selectedOutcome === conn.outcome) &&
            (selectedTerm === null || selectedTerm === conn.term);
          
          const outcomeColor = outcomeColors[conn.outcome];
          // 使用更细的线条
          const logP = -Math.log10(conn.p_value + 1e-300);
          const strokeWidth = Math.max(logP * 0.15, 0.5); // 减小比例因子
          
          const controlX1 = sourcePos.x + (isPositive ? 250 : -250);
          const controlY1 = sourcePos.y;
          const controlX2 = targetPos.x + (isPositive ? -80 : 80);
          const controlY2 = targetPos.y;

          return (
            <path
              key={index}
              d={`M ${sourcePos.x} ${sourcePos.y} C ${controlX1} ${controlY1}, ${controlX2} ${controlY2}, ${targetPos.x} ${targetPos.y}`}
              stroke={outcomeColor}
              strokeWidth={strokeWidth}
              fill="none"
              opacity={isHighlighted ? 0.7 : 0.15}
              className="transition-opacity duration-200"
            />
          );
        })}

        {outcomes.map((outcome, index) => {
          const pos = getOutcomePosition(index);
          const isSelected = selectedOutcome === outcome;
          
          const shortName = outcome.replace('_intake', '').replace('_milk', '').replace('fullfat', 'full').replace('lowfat', 'low');
          
          return (
            <g key={outcome}>
              <circle
                cx={pos.x}
                cy={pos.y}
                r="25"
                fill={outcomeColors[outcome]}
                stroke={isSelected ? "#000" : "#fff"}
                strokeWidth="2"
                className="cursor-pointer transition-all duration-200"
                onMouseEnter={() => setSelectedOutcome(outcome)}
                onMouseLeave={() => setSelectedOutcome(null)}
              />
              <text
                x={pos.x}
                y={pos.y + 4}
                textAnchor="middle"
                className="text-xs font-bold fill-white pointer-events-none"
              >
                {shortName}
              </text>
              <text
                x={pos.x + 40}
                y={pos.y + 5}
                textAnchor="start"
                className="text-sm font-medium fill-gray-700 pointer-events-none"
              >
                {outcome.replace('_', ' ')}
              </text>
            </g>
          );
        })}

        <text x={50} y={130} className="text-sm font-bold fill-red-600">
          Negative Association (estimate &lt; 0)
        </text>
        
        {termsData.negative.map((termData, index) => {
          const pos = getTermPosition(index, termsData.negative.length, false);
          const isSelected = selectedTerm === termData.term;
          return (
            <g key={`neg_${termData.term}`}>
              <circle
                cx={pos.x}
                cy={pos.y}
                r="3"
                fill="#DC2626"
                className="cursor-pointer"
                onMouseEnter={() => setSelectedTerm(termData.term)}
                onMouseLeave={() => setSelectedTerm(null)}
              />
              <text
                x={pos.x - 10}
                y={pos.y + 3}
                textAnchor="end"
                className={`text-xs ${isSelected ? 'font-bold fill-red-600' : 'fill-gray-600'} cursor-pointer`}
                onMouseEnter={() => setSelectedTerm(termData.term)}
                onMouseLeave={() => setSelectedTerm(null)}
              >
                {termData.term.length > 25 ? termData.term.substring(0, 22) + '...' : termData.term}
              </text>
            </g>
          );
        })}

        <text x={rightTermsX + 50} y={130} className="text-sm font-bold fill-green-600">
          Positive Association (estimate &gt; 0)
        </text>
        
        {termsData.positive.map((termData, index) => {
          const pos = getTermPosition(index, termsData.positive.length, true);
          const isSelected = selectedTerm === termData.term;
          return (
            <g key={`pos_${termData.term}`}>
              <circle
                cx={pos.x}
                cy={pos.y}
                r="3"
                fill="#059669"
                className="cursor-pointer"
                onMouseEnter={() => setSelectedTerm(termData.term)}
                onMouseLeave={() => setSelectedTerm(null)}
              />
              <text
                x={pos.x + 10}
                y={pos.y + 3}
                textAnchor="start"
                className={`text-xs ${isSelected ? 'font-bold fill-green-600' : 'fill-gray-600'} cursor-pointer`}
                onMouseEnter={() => setSelectedTerm(termData.term)}
                onMouseLeave={() => setSelectedTerm(null)}
              >
                {termData.term.length > 25 ? termData.term.substring(0, 22) + '...' : termData.term}
              </text>
            </g>
          );
        })}
      </svg>

      {(selectedOutcome || selectedTerm) && (
        <div className="mt-6 p-4 bg-gray-50 rounded-lg no-print">
          <h3 className="text-lg font-semibold mb-2">
            {selectedOutcome && `Selected Outcome: ${selectedOutcome}`}
            {selectedTerm && `Selected Term: ${selectedTerm}`}
          </h3>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            {data
              .filter(d => 
                (!selectedOutcome || d.outcome === selectedOutcome) &&
                (!selectedTerm || d.term === selectedTerm)
              )
              .map((item, index) => (
                <div key={index} className="text-sm">
                  <strong>{item.outcome}</strong> - {item.term}<br/>
                  Estimate: <span className={item.estimate >= 0 ? 'text-green-600' : 'text-red-600'}>
                    {item.estimate.toFixed(4)}
                  </span><br/>
                  P-value: {item.p_value.toExponential(2)}<br/>
                  Bonferroni: {item.p_value_bonferroni.toExponential(2)}<br/>
                  -log10(p): {(-Math.log10(item.p_value)).toFixed(1)}
                </div>
              ))}
          </div>
        </div>
      )}

      <div className="mt-4 text-xs text-gray-600 print:text-xs">
        <p>
          <strong>Dietary Intake Associations Chart (Bonferroni corrected).</strong> 
          Shows 183 significant associations after Bonferroni multiple testing correction. 
          Line thickness is proportional to -log10(p-value). 
          Left side: negative associations, Right side: positive associations.
        </p>
      </div>
    </div>
  );
};

export default ChordDiagram;