entity StateMachine is
  port( X, Y, CLK : in bit;
        Z : out bit);
end StateMachine;

architecture stateProcess of StateMachine is
signal State: integer range 0 to 3 := 0;
begin
  process(CLK)
  begin
  if CLK'event and CLK = '1' then   -- rising clock edge trigger
    case State is
    when 0 =>                       -- next state for current state = 0
      if X&Y = "00" then State <= 0;     
      elsif X&Y = "01" then State <= 1;
      elsif X&Y = "10" then State <= 2;
      else State <= 1;
      end if;
  
    when 1 =>                       -- next state for current state = 1
      if X&Y = "00" then State <= 1;
      elsif X&Y = "01" then State <= 0;
      elsif X&Y = "10" then State <= 2;
      else State <= 3;
      end if;
    
    when 2 =>                       -- next state for current state = 2
      if X&Y = "00" then State <= 2;
      elsif X&Y = "01" then State <= 3;
      elsif X&Y = "10" then State <= 3;
      else State <= 1;
      end if;
    
    when 3 =>                       -- next state for current state = 3
      if X&Y = "00" then State <= 3;
      elsif X&Y = "01" then State <= 0;
      elsif X&Y = "10" then State <= 1;
      else State <= 0;
      end if;
    
  end case;
end if;
end process;

-- Z output values, depending only on state
Z <= '1' when (State = 2 or State = 3) else '0';

end stateProcess;